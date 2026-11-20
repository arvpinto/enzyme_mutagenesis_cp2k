<br>

<h2> <p align="center"> <b>II - Mutagenesis scan</b> </p></h2>

<div align="center">
    <img src="assets/img/mutagenesis_scan.webp" width="380" alt="Close-up of a helix around the reacting atoms with one residue mutated, highlighted in red">
</div>

<br>

<p align="justify"><b>This protocol is designed to perform a mutagenesis scan with residues such as ALA or GLY (other residues are also allowed). The mutated residues are optimized while keeping the rest of the system fixed, and then single-point calculations are performed on the provided reactant and transition state structures. </b></p>

<p align="justify">The protocol requires the following inputs:</p>

<ul>
<li><p>file listing the residues to be mutated;</p></li>
<li><p>topology file in *prmtop format;</p></li>
<li><p>reactant and transition state structures in *pdb format;</p></li>
<li><p>LEaP and CP2K input templates;</p></li>
<li><p>selection of the QM region.</p></li>
</ul>

<p align="justify">Additionally, the following software packages are needed:</p>

<ul>
<li><p>CP2K;</p></li>
<li><p>CPPTRAJ (from AmberTools);</p></li>
<li><p>LEaP (from AmberTools);</p></li>
<li><p>ParmEd (from AmberTools);</p></li>
<li><p>Python 3 with NumPy and Matplotlib.</p></li>
</ul>

---

<br>
<h3><p align="center"> <b> Input preparation </b></p></h3>

<br/>

<p align="justify">To properly construct the QM system, a selection in the format of qm_selection.dat is required. Note that atom numbers will change upon mutation, while residue IDs (resid's) remain the same. The preparation of the selection is explained in the I - Input Preparation section of the <a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/residue_deletion.html" target="_blank">residue deletion protocol</a>.</p>

<br>

The <a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/scripts/mut_scan_qmmm_cp2k.sh" target="_blank">mut_scan_qmmm_cp2k.sh</a> script has the following usage:

<pre style="color: white; background-color: black;">
<span style="color: #FF3366; font-weight: bold;">mut_scan_qmmm_cp2k.sh [options] &lt;residue_list&gt; &lt;scan_type&gt; &lt;topology&gt; &lt;reactant_structure&gt; &lt;ts_structure&gt; &lt;selection&gt; &lt;leap_template&gt; &lt;cp2k_template&gt; &lt;qm_selection&gt;</span>

<span style="color: #9966CC;">user@machine:~$</span> mut_scan_qmmm_cp2k.sh --qmmm-reference forceeval_qmmm.inc scan_list.dat GLY hpla2_ee.prmtop R.pdb TS.pdb :1-124 leap_template.in cp2k_template.inp qm_selection.dat
</pre>

<p align="justify">It prepares a directory for each residue in the list where the input files for CP2K will be output, and a WT directory with the single-points of the unmodified system. The &lt;scan_type&gt; argument represents the three-letter code of the residue used in the scan. ALA and GLY are suitable for assessing sidechain contributions, whereas residues with greater volume and more degrees of freedom may complicate the analysis. Note: when mutating a CYX residue in a disulfide bridge, the other CYX is converted to CYS and is optimized together with the mutated residue. The &lt;selection&gt; argument defines the residue range of the enzyme, which has to be the first block of atoms of the topology (its parameters are updated, while the rest of the system remains unchanged). The cp2k_template.inp file is used to produce geometry optimization and single-point energy input files. The scan_summary.dat file reports what was done to each residue.</p>

<p align="justify">The LEaP input has to load the same force field that was used for the original topology (the sample topology was built with ff99SB-ILDN, hence the <code>source oldff/leaprc.ff99SBildn</code> line of the sample leap_template.in). The scripts verify this: if the atom types of the residues that were not mutated differ from the original topology the mutant is not built. In addition, the charges and Lennard-Jones parameters of all the residues that were not mutated are copied from the original topology, so that changes made to it after LEaP (e.g. charges of residues at the QM/MM boundary, or Lennard-Jones parameters of hydroxyl hydrogens) are kept, and the mutant differs from the wild type only in the mutated residue.</p>

<br>

<p align="justify">To generate the structures and topology of the mutated enzyme, the <a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/scripts/build_mutant.sh" target="_blank">build_mutant.sh</a> script is called by the mut_scan_qmmm_cp2k.sh script and has the following syntax:</p>

<pre style="color: white; background-color: black;">
<span style="color: #FF3366; font-weight: bold;">build_mutant.sh [options] &lt;mut_name&gt; &lt;topology&gt; &lt;reactant_structure&gt; &lt;ts_structure&gt; &lt;selection&gt; &lt;leap_template&gt; &lt;mutation&gt; [&lt;mutation&gt; ...]</span>
</pre>

<p align="justify">The mutated residues keep their backbone atoms (and CB, unless GLY is involved), and LEaP rebuilds the rest of each residue from its template. The <code>place-sidechains</code> subcommand of <a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/scripts/qmmm_mut_tools.py" target="_blank">qmmm_mut_tools.py</a> then samples the sidechain torsions of the new residues (staggered values for sp<sup>3</sup> torsions, 30° steps for planar groups, followed by a local refinement) and keeps the conformation with the fewest steric overlaps with the rest of the system, including the solvent and the substrate. This is done separately for the reactant and the transition state, and the result of each residue is written to placement.log in the mutant directory. The <code>--no-placement</code> option keeps the geometry built by LEaP.</p>


<br>

<p align="justify">Since mutating residues changes the atom numbering, the QM/MM settings must be updated for each mutation. The <a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/scripts/qmmm_mut_tools.py" target="_blank">qmmm_mut_tools.py</a> script is called by the mut_scan_qmmm_cp2k.sh script to check how the mutated residue should be included in the QM layer and modify the qm_selection.dat file accordingly (the parts of the backbone and of the sidechain that were in the QM layer remain there). It has the following usage:</p>

<pre style="color: white; background-color: black;">
<span style="color: #FF3366; font-weight: bold;">qmmm_mut_tools.py mutate-selection -w &lt;topology&gt; -m &lt;mutant_topology&gt; -q &lt;qm_selection&gt; -o &lt;new_qm_selection&gt; &lt;mutation&gt; [&lt;mutation&gt; ...]</span>
</pre>
 
<p align="justify">The <code>qmmm-section</code> subcommand of <a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/scripts/qmmm_mut_tools.py" target="_blank">qmmm_mut_tools.py</a> then writes a file with the configuration of the QM layer (QM cell, QM atoms and link atoms), defined by the selection in the qm_selection.dat file. The resulting QM layer is then checked against the topology (link atoms, charge, number of electrons), and no inputs are written for a residue that fails these checks.</p>

<p align="justify">By default, the QM cell is built around the QM atoms of each system (5 Å on each side) and the link atom parameters (<code>ALPHA_IMOMM</code> and <code>QMMM_SCALE_FACTOR</code>) are estimated from the charges of the MM atoms. If the QM/MM section used to obtain the original reaction path is available, it should be given with <code>--qmmm-reference forceeval_qmmm.inc</code> (the file used for this example is in the hpla2_example folder). The QM cell is then copied from that file, and every link atom that also exists in the original system keeps its original parameters, so that the WT single points reproduce the original setup and all mutants share the same QM cell. Only links that did not exist before (for instance, around a mutated residue of the QM layer) get estimated parameters, and they are listed in the qmmm_section.log file. A system whose QM atoms do not fit in the reference cell (less than 2 Å from its edges, see <code>--min-margin</code>) is skipped.</p>

<br/>

The calculations can then be run using a for loop:
<pre style="color: white; background-color: black;">
<span style="color: #9966CC;">user@machine:~$</span> cd WT ; cp2k.popt -i sp_res_R.inp -o sp_res_R.out ; cp2k.popt -i sp_res_TS.inp -o sp_res_TS.out ; cd ..
<span style="color: #9966CC;">user@machine:~$</span> for i in GLY_*; do cd "$i" ; cp2k.popt -i opt_res_R.inp -o opt_res_R.out ; cp2k.popt -i sp_res_R.inp -o sp_res_R.out ; cp2k.popt -i opt_res_TS.inp -o opt_res_TS.out ; cp2k.popt -i sp_res_TS.inp -o sp_res_TS.out ; cd .. ; done
</pre>

<br/>

<h3><p align="center"> <b> Output processing </b></p></h3>

<br>

After running the optimization and single-point calculations, the <a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/scripts/extract_barriers.py" target="_blank">extract_barriers.py</a> script extracts the absolute energies and calculates the R->TS energy barrier for each residue mutation and for the unmodified system:

<pre style="color: white; background-color: black;">
<span style="color: #9966CC;">user@machine:~$</span> extract_barriers.py --wt-dir WT -o energy_differences_scan.dat GLY_*
</pre>

<p align="justify">Entries whose optimization stopped at the maximum number of steps without converging are reported by the script, and left out with the <code>--strict-opt</code> option.</p>

<br/>

<p align="justify">The energy barriers can be compared to the ones calculated with the <a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/residue_deletion.html" target="_blank">residue deletion protocol</a> using the <a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/scripts/E_diff_comparison_bar_plot.py" target="_blank">E_diff_comparison_bar_plot.py</a> script:</p>

<pre style="color: white; background-color: black;">
<span style="color: #9966CC;">user@machine:~$</span> E_diff_comparison_bar_plot.py energy_differences_del.dat energy_differences_scan.dat --labels "residue deletion" "mutation to GLY"
</pre>

<div align="center">
    <img src="hpla2_example/comparison_bar_plot.png">
</div>

<p align="justify"> The energy barriers exhibit a similar trend in both cases. Differences between the two protocols are expected, since the deletion removes the backbone and leaves a cavity, while the mutation by GLY keeps the backbone and is followed by a geometry optimization. For CYX43 the two protocols also differ in the remaining cysteine of the disulfide bridge: upon deletion it keeps the CYX parameters with an unsaturated sulfur atom, whereas the mutation yields a protonated cysteine. As expected, either deletion or mutation of the catalytic ASP91 results in a significantly higher energy barrier. Residues that significantly lower the energy barrier become key hotspots for targeted mutagenesis studies. It is important to note that different conformations in the R and TS states might break the adiabatic connection and lead to jumps in the PES.</p>

<p align="justify"> Note: the two sets of values can only be compared if both were calculated relative to the same unmodified system, which is why the mutant topologies keep the parameters of the original topology for every residue that was not mutated.</p>

<br/>
