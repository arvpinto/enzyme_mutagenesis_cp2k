#!/usr/bin/env python3
"""
render_md_movie.py

Movie of the classical MD of a multipoint variant (Section III): the enzyme as a
cartoon, the mutated residues in sticks with their labels (e.g. D41K), the other
atoms that move in the MD (side chains and waters around the mutations) as thin
sticks, and the QM region in ball-and-stick, drawn by 3Dmol.js in headless Chrome
and encoded with ffmpeg as H.264 MP4, in a dark and a light version, plus a
poster image of the first frame (same look as the movies of enzyme_neb_cp2k).

The trajectory is the DCD file written by the md_res input of a mutant directory
(<PROJECT>-pos-1.dcd). Atoms that never move in it (the fixed atoms) are found
from the trajectory itself.

Needs: ParmEd, NumPy, Pillow, playwright (Python) with a Chrome/Chromium browser,
and ffmpeg. 3Dmol.js is read from tools/3Dmol-min.js.

Example (from the mutant directory of mut_mp_qmmm_cp2k.sh)
    python3 tools/render_md_movie.py -p MUT_004/MUT_004.prmtop -t MUT_004/MUT_SCAN_MD_R-pos-1.dcd \\
        --wt-topology hpla2_example/hpla2_ee.prmtop --qmmm-inc MUT_004/forceeval_qmmm.inc \\
        --residues 41,53,74,92,51,100 --name multi_md --chrome /usr/bin/google-chrome
"""

import argparse
import asyncio
import base64
import re
import shutil
import struct
import subprocess
import sys
import tempfile
from pathlib import Path

import numpy as np

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent

AMINO = {"ALA", "ARG", "ASN", "ASP", "ASH", "CYS", "CYX", "CYM", "GLN", "GLU", "GLH", "GLY", "HIS",
         "HID", "HIE", "HIP", "ILE", "LEU", "LYS", "LYN", "MET", "PHE", "PRO", "SER", "THR", "TRP",
         "TYR", "VAL"}
ONE = {"ALA": "A", "ARG": "R", "ASN": "N", "ASP": "D", "ASH": "D", "CYS": "C", "CYX": "C", "CYM": "C",
       "GLN": "Q", "GLU": "E", "GLH": "E", "GLY": "G", "HIS": "H", "HID": "H", "HIE": "H", "HIP": "H",
       "ILE": "I", "LEU": "L", "LYS": "K", "LYN": "K", "MET": "M", "PHE": "F", "PRO": "P", "SER": "S",
       "THR": "T", "TRP": "W", "TYR": "Y", "VAL": "V"}
WATER = {"WAT", "HOH", "TIP3", "SOL"}

PAGE = """<!DOCTYPE html><html><head><meta charset="utf-8">
<style>html,body{margin:0;padding:0;background:#000}#v{position:relative;width:%(width)dpx;height:%(height)dpx}</style>
<script>%(lib)s</script></head>
<body><div id="v"></div></body></html>"""

# Colours of the two versions: background, cartoon, mutated residues, QM carbons,
# carbons of the other moving atoms, labels (VMD-like element colours, as in the
# movies of enzyme_neb_cp2k).
PALETTE = {
    "dark": {"bg": "#0e0f14", "cartoon": "#c9ced6", "mut": "#ff3fb4", "qm": "#19dcef", "free": "#8b929c",
             "H": "#eef0f5", "N": "#3f62ff", "O": "#ec2629", "S": "#f2d14b", "P": "#d9a95f", "Ca": "#7ad67e",
             "label": "#0e0f14", "labelBg": "#ff3fb4", "text": "#eef2f6"},
    "light": {"bg": "#f4f6f9", "cartoon": "#9aa2ad", "mut": "#d4148a", "qm": "#00a3b5", "free": "#6b727c",
              "H": "#d5dae1", "N": "#2443e0", "O": "#d61f22", "S": "#c9a400", "P": "#c08a3c", "Ca": "#3fa34d",
              "label": "#ffffff", "labelBg": "#d4148a", "text": "#12161c"},
}

DRAW = r"""(a) => {
  const V = window.V, P = a.pal, S = window.SCENE;
  V.clear();
  V.setBackgroundColor(P.bg);
  const m = V.addModel(a.pdb, 'pdb', {keepH: true});
  const el = (c) => { const o = {H: P.H, N: P.N, O: P.O, S: P.S, P: P.P, Ca: P.Ca, CA: P.Ca, C: c};
                      return {prop: 'elem', map: o}; };
  const fix = (atoms) => atoms.forEach((x) => { if (x.elem === 'CA' || x.elem === 'Ca') x.elem = 'Ca'; });
  fix(m.selectedAtoms({}));
  m.setStyle({}, {});
  m.setStyle({chain: 'P'}, {cartoon: {color: P.cartoon, thickness: 0.4, opacity: 1.0}});
  m.setStyle({chain: 'P', resi: S.mut}, {cartoon: {color: P.mut, thickness: 0.4}});
  m.setStyle({chain: 'P', resi: S.mut, not: {atom: S.backbone}},
             {stick: {radius: 0.3, colorscheme: el(P.mut)}}, true);
  m.setStyle({chain: 'F'}, {stick: {radius: 0.11, colorscheme: el(P.free)}}, true);
  m.setStyle({chain: 'Q'}, {stick: {radius: 0.1, colorscheme: el(P.qm)},
                            sphere: {scale: 0.16, colorscheme: el(P.qm)}}, true);
  m.setStyle({chain: 'I'}, {sphere: {radius: 0.9, color: P.Ca}});
  S.labels.forEach((l, k) => {
    V.addLabel(l.text, {position: a.lab[k], fontSize: a.fontSize, fontColor: P.label,
                        backgroundColor: P.labelBg, backgroundOpacity: 0.9, borderRadius: 5,
                        padding: 2, inFront: true, alignment: 'center'});
  });
  if (a.first) { V.zoomTo({chain: 'P'}); V.zoom(a.zoom); window.VIEW = V.getView(); }
  else { V.setView(window.VIEW); }
  V.render();
}"""


def read_dcd(path):
    """Coordinates (nframes, natoms, 3) and the step interval between frames of a CHARMM/CP2K DCD file."""
    data = Path(path).read_bytes()
    pos = 0

    def record():
        nonlocal pos
        (n,) = struct.unpack_from("<i", data, pos)
        body = data[pos + 4:pos + 4 + n]
        (m,) = struct.unpack_from("<i", data, pos + 4 + n)
        if m != n:
            sys.exit(f"error: {path} is not a little-endian DCD file with 4-byte record markers")
        pos += 8 + n
        return body

    head = record()
    if head[:4] != b"CORD":
        sys.exit(f"error: {path} does not start with a CORD header")
    icntrl = struct.unpack_from("<20i", head, 4)
    nsavc = icntrl[2]
    has_cell = icntrl[10] != 0
    record()                                    # title
    (natom,) = struct.unpack("<i", record())
    frames = []
    while pos < len(data):
        try:
            if has_cell:
                record()
            x = np.frombuffer(record(), dtype="<f4")
            y = np.frombuffer(record(), dtype="<f4")
            z = np.frombuffer(record(), dtype="<f4")
        except struct.error:
            break                               # truncated last frame (run still going)
        if len(x) != natom or len(y) != natom or len(z) != natom:
            break
        frames.append(np.stack([x, y, z], axis=1).astype(float))
    if not frames:
        sys.exit(f"error: no frames in {path}")
    return np.array(frames), nsavc


def qm_atoms_from_inc(path):
    """0-based indexes of the QM atoms (MM_INDEX lines of the &QM_KIND sections)."""
    idx = []
    inside = False
    for line in Path(path).read_text().splitlines():
        s = line.split("!")[0].strip()
        u = s.upper()
        if u.startswith("&QM_KIND"):
            inside = True
        elif u.startswith("&END QM_KIND"):
            inside = False
        elif inside and u.startswith("MM_INDEX"):
            idx += [int(t) - 1 for t in s.split()[1:]]
    return sorted(set(idx))


def element(atom):
    z = atom.atomic_number
    names = {1: "H", 6: "C", 7: "N", 8: "O", 15: "P", 16: "S", 20: "Ca", 11: "Na", 17: "Cl"}
    if z in names:
        return names[z]
    return re.sub(r"[^A-Za-z]", "", atom.name)[:1].upper() or "X"


def view_rotation(protein_xyz, focus_xyz):
    """Rotation that puts the protein centre at the origin, the mutated residues towards
    the viewer (+z) and the longest axis of the protein across the screen (x)."""
    c = protein_xyz.mean(axis=0)
    d = focus_xyz.mean(axis=0) - c
    if np.linalg.norm(d) < 1e-6:
        d = np.array([0.0, 0.0, 1.0])
    zax = d / np.linalg.norm(d)
    X = protein_xyz - c
    X = X - np.outer(X @ zax, zax)              # projection on the screen plane
    w, v = np.linalg.eigh(X.T @ X)
    xax = v[:, -1] - (v[:, -1] @ zax) * zax
    xax /= np.linalg.norm(xax)
    yax = np.cross(zax, xax)
    return c, np.array([xax, yax, zax])


def frame_pdb(sel, chains, meta, xyz):
    out = []
    for n, i in enumerate(sel):
        name, resn, resi, el = meta[i]
        nm = f" {name:<3s}" if len(name) < 4 else name[:4]
        x, y, z = xyz[i]
        out.append(f"ATOM  {n + 1:5d} {nm} {resn[:3]:>3s} {chains[n]}{resi:4d}    "
                   f"{x:8.3f}{y:8.3f}{z:8.3f}  1.00  0.00          {el:>2s}")
    out.append("END")
    return "\n".join(out) + "\n"


async def render(page_html, jobs, width, height, chrome):
    from playwright.async_api import async_playwright
    async with async_playwright() as p:
        kw = {"args": ["--use-gl=angle", "--use-angle=swiftshader", "--enable-unsafe-swiftshader",
                       "--ignore-gpu-blocklist"]}
        if chrome:
            kw["executable_path"] = chrome
        browser = await p.chromium.launch(**kw)
        page = await browser.new_page(viewport={"width": width, "height": height}, device_scale_factor=1)
        await page.set_content(page_html)
        await page.evaluate("() => { window.V = $3Dmol.createViewer(document.getElementById('v'), {antialias: true}); }")
        for job in jobs:
            await page.evaluate("(s) => { window.SCENE = s; }", job["scene"])
            for k, (pdb, lab) in enumerate(zip(job["pdbs"], job["labels"])):
                await page.evaluate(DRAW, {"pdb": pdb, "lab": lab, "pal": job["pal"], "first": k == 0,
                                           "zoom": job["zoom"], "fontSize": job["font_size"]})
                png = await page.evaluate("() => window.V.pngURI()")
                Path(job["dir"], f"r{k:04d}.png").write_bytes(base64.b64decode(png.split(",", 1)[1]))
        await browser.close()


def stamp(src, dst, text, colour, font, size):
    from PIL import Image, ImageDraw, ImageFont
    im = Image.open(src).convert("RGB")
    try:
        f = ImageFont.truetype(font, size)
        try:
            f.set_variation_by_axes([600])
        except Exception:
            pass
    except Exception:
        f = ImageFont.load_default()
    d = ImageDraw.Draw(im)
    d.text((24, im.height - 24), text, fill=colour, font=f, anchor="ls")
    im.save(dst)


def main():
    ap = argparse.ArgumentParser(description="Movie of the classical MD of a multipoint variant (3Dmol.js + ffmpeg).",
                                 formatter_class=argparse.RawDescriptionHelpFormatter, epilog=__doc__.split("Example", 1)[1])
    ap.add_argument("-p", "--topology", required=True, help="prmtop of the variant (MUT_xxx.prmtop)")
    ap.add_argument("-t", "--trajectory", required=True, help="DCD of the classical MD (md_res input)")
    ap.add_argument("--residues", required=True, help="comma-separated numbers of the mutated residues")
    ap.add_argument("--wt-topology", help="prmtop of the unmodified enzyme, for labels such as D41K (default: new residue only)")
    ap.add_argument("--qmmm-inc", help="forceeval_qmmm.inc of the variant: its QM atoms are drawn in ball-and-stick")
    ap.add_argument("--timestep", type=float, default=1.0, help="MD time step in fs (default: 1.0)")
    ap.add_argument("--every", type=int, default=1, help="use one DCD frame out of N (default: 1)")
    ap.add_argument("--hold", type=float, default=0.6, help="seconds the last frame is held before the loop restarts (default: 0.6)")
    ap.add_argument("--move-threshold", type=float, default=0.05,
                    help="atoms that move more than this (A) in the trajectory are drawn as free (default: 0.05)")
    ap.add_argument("--out-dir", default=str(ROOT / "assets/media"), help="output folder (default: assets/media)")
    ap.add_argument("--name", default="multi_md", help="base name of the files (default: multi_md)")
    ap.add_argument("--width", type=int, default=960, help="movie width in pixels (default: 960)")
    ap.add_argument("--height", type=int, default=640, help="movie height in pixels (default: 640)")
    ap.add_argument("--fps", type=int, default=12, help="frames per second (default: 12)")
    ap.add_argument("--zoom", type=float, default=1.35, help="zoom after fitting the protein (default: 1.35)")
    ap.add_argument("--label-offset", type=float, default=4.0,
                    help="distance (A) between the tip of a mutated side chain and its label (default: 4.0)")
    ap.add_argument("--font", default=None, help="TTF font of the time stamp (default: DejaVu Sans)")
    ap.add_argument("--crf", type=int, default=22, help="x264 quality (default: 22)")
    ap.add_argument("--chrome", default=None, help="Chrome/Chromium executable (default: the one of playwright)")
    ap.add_argument("--only", choices=["dark", "light"], help="render one version only")
    args = ap.parse_args()

    if not shutil.which("ffmpeg"):
        sys.exit("error: ffmpeg not found")
    try:
        import parmed
    except ImportError:
        sys.exit("error: ParmEd is needed")
    lib_path = HERE / "3Dmol-min.js"
    if not lib_path.is_file():
        sys.exit(f"error: {lib_path} not found")

    top = parmed.load_file(args.topology)
    traj, nsavc = read_dcd(args.trajectory)
    if traj.shape[1] != len(top.atoms):
        sys.exit(f"error: {traj.shape[1]} atoms in {args.trajectory}, {len(top.atoms)} in {args.topology}")
    traj = traj[::args.every]
    step_fs = (nsavc or 1) * args.every * args.timestep

    mut = sorted({int(x) for x in args.residues.replace(" ", "").split(",") if x})
    wt_names = {}
    if args.wt_topology:
        wt = parmed.load_file(args.wt_topology)
        wt_names = {r.idx + 1: r.name for r in wt.residues}
    meta, prot, mut_atoms = [], [], []
    for a in top.atoms:
        r = a.residue
        meta.append((a.name, r.name, r.idx + 1, element(a)))
        if r.name in AMINO:
            prot.append(a.idx)
            if r.idx + 1 in mut:
                mut_atoms.append(a.idx)
    if not prot:
        sys.exit("error: no protein residues in the topology")
    missing = [n for n in mut if n > len(top.residues) or top.residues[n - 1].name not in AMINO]
    if missing:
        sys.exit(f"error: residues {missing} are not amino acids of {args.topology}")

    disp = np.abs(traj - traj[0]).max(axis=(0, 2))
    moving = set(np.nonzero(disp > args.move_threshold)[0].tolist())
    qm = set(qm_atoms_from_inc(args.qmmm_inc)) if args.qmmm_inc else set()
    prot_set, mut_set = set(prot), set(mut_atoms)

    # chains: P protein (cartoon; mutated residues also in sticks), Q QM atoms that are not
    # protein, F moving atoms outside the protein and moving side chains of other residues,
    # I calcium ions. Protein atoms of the QM region are also added as chain Q.
    sel, chains = [], []
    for i in prot:
        sel.append(i); chains.append("P")
    for i in sorted(qm):
        if meta[i][3] == "Ca":
            continue
        sel.append(i); chains.append("Q")
    backbone = {"N", "H", "CA", "HA", "C", "O", "HA2", "HA3"}
    for i in sorted(moving - mut_set - qm):
        if i in prot_set and meta[i][0] in backbone:
            continue
        if i in prot_set or meta[i][1] in WATER or meta[i][1] not in AMINO:
            sel.append(i); chains.append("F")
    ions = [a.idx for a in top.atoms if element(a) == "Ca" and (a.idx in qm or a.idx in moving or
            np.linalg.norm(traj[0][a.idx] - traj[0][prot].mean(axis=0)) < 30)]
    for i in ions:
        sel.append(i); chains.append("I")

    centre, R = view_rotation(traj[0][prot], traj[0][mut_atoms])
    frames = [(x - centre) @ R.T for x in traj]

    labels = []
    for n in mut:
        res = top.residues[n - 1]
        new = ONE.get(res.name, res.name)
        old = ONE.get(wt_names.get(n, ""), "")
        labels.append({"text": f"{old}{n}{new}" if old else f"{res.name}{n}", "resi": n})
    lab_atoms = []
    for n in mut:
        res = top.residues[n - 1]
        tip = max(res.atoms, key=lambda a: np.linalg.norm(traj[0][a.idx] - traj[0][[b.idx for b in res.atoms if b.name == "CA"][0]]))
        lab_atoms.append(tip.idx)

    pdbs = [frame_pdb(sel, chains, meta, x) for x in frames]
    def label_at(x, i):
        # beyond the tip of the side chain, away from the centre of the protein on the screen
        v = x[i][:2] - x[prot][:, :2].mean(axis=0)
        v = v / (np.linalg.norm(v) or 1.0)
        return {"x": float(x[i][0] + args.label_offset * v[0]), "y": float(x[i][1] + args.label_offset * v[1]),
                "z": float(x[i][2] + 3.0)}
    lab_pos = [[label_at(x, i) for i in lab_atoms] for x in frames]
    scene = {"mut": mut, "backbone": ["N", "H", "C", "O", "HA"], "labels": labels}

    out_dir = Path(args.out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    html = PAGE % {"width": args.width, "height": args.height, "lib": lib_path.read_text()}
    tmp = Path(tempfile.mkdtemp(prefix="mdmovie_"))
    jobs = []
    for mode in ("dark", "light"):
        if args.only and mode != args.only:
            continue
        d = tmp / mode
        d.mkdir()
        jobs.append({"mode": mode, "dir": str(d), "pdbs": pdbs, "labels": lab_pos, "scene": scene,
                     "pal": PALETTE[mode], "zoom": args.zoom, "font_size": max(12, args.width // 56)})
    print(f"{len(traj)} frames, {step_fs:g} fs apart ({(len(traj) - 1) * step_fs / 1000:.2f} ps); "
          f"{len(sel)} atoms drawn ({len(moving)} move, {len(qm)} QM); mutations: "
          + " ".join(l["text"] for l in labels))
    asyncio.run(render(html, jobs, args.width, args.height, args.chrome))

    font = args.font or "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
    hold = max(0, round(args.hold * args.fps))
    for job in jobs:
        d = Path(job["dir"])
        n = len(pdbs)
        k_out = 0
        for k in list(range(n)) + [n - 1] * hold:
            stamp(d / f"r{k:04d}.png", d / f"f{k_out:04d}.png", f"t = {k * step_fs / 1000:.2f} ps",
                  PALETTE[job["mode"]]["text"], font, max(14, args.width // 24))
            k_out += 1
        mp4 = out_dir / f"{args.name}_{job['mode']}.mp4"
        subprocess.run(["ffmpeg", "-y", "-loglevel", "error", "-framerate", str(args.fps), "-i", str(d / "f%04d.png"),
                        "-vf", f"scale={args.width}:{args.height}:flags=lanczos", "-c:v", "libx264", "-preset", "veryfast", "-crf", str(args.crf), "-pix_fmt", "yuv420p",
                        "-movflags", "+faststart", str(mp4)], check=True)
        poster = out_dir / f"{args.name}_{job['mode']}.jpg"
        subprocess.run(["ffmpeg", "-y", "-loglevel", "error", "-i", str(d / "f0000.png"),
                        "-vf", f"scale={args.width}:{args.height}:flags=lanczos", "-q:v", "3", str(poster)],
                       check=True)
        print(f"written {mp4} ({mp4.stat().st_size / 1024:.0f} kB) and {poster}")
    shutil.rmtree(tmp)


if __name__ == "__main__":
    main()
