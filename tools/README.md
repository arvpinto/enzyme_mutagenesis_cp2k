# tools

Script that renders the movie of Section III (multipoint mutagenesis); it is not
needed to run the protocols.

| File | Output |
|---|---|
| `render_md_movie.py` | `assets/media/multi_md_{dark,light}.mp4` and their posters (`.jpg`): the classical MD of a variant, with the mutated residues labelled (needs ParmEd, NumPy, Pillow, Playwright with Chrome/Chromium, and ffmpeg) |
| `3Dmol-min.js` | [3Dmol.js](https://3dmol.csb.pitt.edu/), used by the script to draw the frames (BSD 3-Clause licence in `3Dmol-LICENSE.txt`) |

The movie on the page was made from the reactant of MUT_004, prepared with
`mut_mp_qmmm_cp2k.sh` from the files in `hpla2_example/`, after the classical
optimization (`opt_md_res_R.inp`), with the first 1500 steps of `md_res_R.inp`
and a frame every 25 steps:

```bash
python3 tools/render_md_movie.py -p MUT_004/MUT_004.prmtop -t MUT_004/MUT_SCAN_MD_R-pos-1.dcd \
    --wt-topology hpla2_example/hpla2_ee.prmtop --qmmm-inc MUT_004/forceeval_qmmm.inc \
    --residues 41,53,74,92,51,100 --chrome /usr/bin/google-chrome
```

The DCD written by the protocol (`md_res_R.inp`, a frame every 100 steps over 5 ps)
can be used in the same way.
