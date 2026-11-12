<br>

<h2><p align="center"> <b>Multipoint Mutagenesis</b> </p> </h2>

<p align="justify"><b>This protocol is designed to evaluate activation energies of multipoint mutant variants. The mutated residues are submitted to molecular dynamics and geometry optimization to produce a more natural conformation while keeping the rest of the system fixed. </b></p>

<p><b>The protocol requires the following inputs:</b>
<br>
● File listing the mutations in the variants
<br>
● Topology file in *prmtop format
<br>
● Reactant and transition state structures in *pdb format
<br>
● LEaP and CP2K input templates
<br>
● Selection of the QM region
<br>
● File listing cpptraj selections of atoms to keep free during GEO_OPT and MD
<br>
<br>
<b>Additionally, the following software packages are needed:</b>
<br>
● CP2K
<br>
● PyMOL
<br>
● VMD
<br>
● CPPTRAJ (from AmberTools)
<br>
● LEaP (from AmberTools)
<br>
● ParmEd (from AmberTools)
<br>
● Python 3 with Matplotlib (for the plots)
</p>


---

<br>
<h2> <p align="center"> <b>I - Input Preparation </b> </p></h2>

<br/>

<p align="justify">A selection in the format presented in qm_selection.dat is required to adequately build the QM system (the atom numbers will change upon mutation, resid's remain the same). The preparation of the selection is explained in the I - Input Preparation section of the <a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/residue_deletion.html" target="_blank">residue deletion protocol</a>.</p>

<br>

The <a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/scripts/mut_mp_qmmm_cp2k.sh" target="_blank">mut_mp_qmmm_cp2k.sh</a> script has the following usage:

<pre style="color: white; background-color: black;">
<span style="color: #FF3366; font-weight: bold;">mut_mp_qmmm_cp2k.sh [options] &lt;mutant_list&gt; &lt;topology&gt; &lt;reactant_structure&gt; &lt;ts_structure&gt; &lt;selection&gt; &lt;leap_template&gt; &lt;cp2k_template&gt; &lt;qm_selection&gt; &lt;selection_free&gt;</span>
    
<span style="color: #9966CC;">user@machine:~$</span> mut_mp_qmmm_cp2k.sh variant_list.dat hpla2_ee.prmtop R.pdb TS.pdb :1-124 leap_template.in cp2k_template.inp qm_selection.dat free_selection.dat
</pre>

<p align="justify">It prepares a directory for each mutant variant in the list where the input files for CP2K will be output, and a WT directory with the single-points of the unmodified system. A given mutant variant can be specified as &lt;name&gt; &lt;number&gt;&lt;residue&gt; in the file mutant variant list (e.g. MUT_001 41LYS 53ASP 74GLU 92ASP). Note: when mutating a CYX residue in a disulfide bridge, the other CYX is converted to CYS. The &lt;selection&gt; argument defines the residue range of the enzyme, which has to be the first block of atoms of the topology (its parameters are updated, while the rest of the system remains unchanged). The LEaP input has to load the same force field that was used for the original topology, as explained in the <a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/mutagenesis_scan.html" target="_blank">mutagenesis scan protocol</a>. The cp2k_template.inp file is used to produce molecular dynamics, geometry optimization and single-point energy input files. The &lt;selection_free&gt; argument requires a file containing cpptraj selections for each mutant variant, specifying the atoms that remain unfrozen (e.g. MUT_001 (:41,53,74,92&amp;!@N,H,CA,HA,C,O)&lt;:3). The selection is evaluated on the transition state structure and the same atoms are free in both states. The mp_summary.dat file reports what was done to each variant.</p>

<p align="justify">The first optimization and the molecular dynamics are classical, and the force field cannot describe the bonds that are being formed and broken in the transition state. For this reason, the atoms of the QM layer that are selected by &lt;selection_free&gt; and do not belong to a mutated residue are kept fixed (they are listed in mp_summary.dat). The <code>--allow-free-qm</code> option turns this off, and should only be used if none of those atoms takes part in the reaction. The length, time step and temperature of the molecular dynamics can be changed with the <code>--md-steps</code>, <code>--timestep</code> and <code>--temperature</code> options.</p>

<br/>
    
<p align="justify">To generate the structures and topology of the mutated enzyme, the <a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/scripts/build_mutant.sh" target="_blank">build_mutant.sh</a> script is called by the mut_mp_qmmm_cp2k.sh script and has the following syntax:</p>

<pre style="color: white; background-color: black;">
<span style="color: #FF3366; font-weight: bold;">build_mutant.sh [options] &lt;mut_name&gt; &lt;topology&gt; &lt;reactant_structure&gt; &lt;ts_structure&gt; &lt;selection&gt; &lt;leap_template&gt; &lt;mutation&gt; [&lt;mutation&gt; ...]</span>
</pre>

<br/>

<p align="justify">Since mutating residues changes the atom numbering, the QM/MM settings must be updated for each mutation. The <a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/scripts/qmmm_mut_tools.py" target="_blank">qmmm_mut_tools.py</a> script checks how the mutated residues should be included in the QM layer and modifies the qm_selection.dat file accordingly. It has the following usage:</p>

<pre style="color: white; background-color: black;">
<span style="color: #FF3366; font-weight: bold;">qmmm_mut_tools.py mutate-selection -w &lt;topology&gt; -m &lt;mutant_topology&gt; -q &lt;qm_selection&gt; -o &lt;new_qm_selection&gt; &lt;mutation&gt; [&lt;mutation&gt; ...]</span>
</pre>

<p align="justify">The <a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/scripts/vmd_forceeval.tcl" target="_blank">vmd_forceeval.tcl</a> script is called within the latter to produce a file with the configuration of the QM layer, defined by the selection in the qm_selection.dat file. The resulting QM layer is then checked against the topology (link atoms, charge, number of electrons), and no inputs are written for a variant that fails these checks.</p>

<br/>

The calculations can then be run using a for loop:
<pre style="color: white; background-color: black;">
<span style="color: #9966CC;">user@machine:~$</span> cd WT ; cp2k.popt -i sp_res_R.inp -o sp_res_R.out ; cp2k.popt -i sp_res_TS.inp -o sp_res_TS.out ; cd ..
<span style="color: #9966CC;">user@machine:~$</span> for i in MUT_*; do cd "$i" ; cp2k.popt -i opt_md_res_R.inp -o opt_md_res_R.out ; cp2k.popt -i md_res_R.inp -o md_res_R.out ; cp2k.popt -i opt_res_R.inp -o opt_res_R.out ; cp2k.popt -i sp_res_R.inp -o sp_res_R.out ; cp2k.popt -i opt_md_res_TS.inp -o opt_md_res_TS.out ; cp2k.popt -i md_res_TS.inp -o md_res_TS.out ; cp2k.popt -i opt_res_TS.inp -o opt_res_TS.out ; cp2k.popt -i sp_res_TS.inp -o sp_res_TS.out ; cd .. ; done
</pre>

<br/>

<div align="center">
    <img src="assets/img/mut_multi_md.gif" width="500">
</div>

<br/>

<h2> <p align="center"> <b>II - Output Processing</b> </p></h2>

<br>

After running the optimization and single-point calculations, the <a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/scripts/extract_barriers.py" target="_blank">extract_barriers.py</a> script extracts the absolute energies and calculates the R->TS energy barrier for each mutant variant and for the unmodified system:

<pre style="color: white; background-color: black;">
<span style="color: #9966CC;">user@machine:~$</span> extract_barriers.py --wt-dir WT -o energy_differences_multi.dat MUT_*
</pre>

<br/>

The energy barriers can be plotted with the <a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/scripts/E_diff_multi_bar_plot.py" target="_blank">E_diff_multi_bar_plot.py</a> script:

<pre style="color: white; background-color: black;">
<span style="color: #9966CC;">user@machine:~$</span> E_diff_multi_bar_plot.py energy_differences_multi.dat
</pre>

<br/>

<p align="justify"> The calculated energy barriers of the mutant variants can be compared with the original energy barrier to see if the introduced mutations stabilize or destabilize the transition state of the reaction step. Since the reactant and the transition state of each variant are relaxed independently, the mutated residues and their surroundings may end up in different conformations in the two states, which adds a contribution to the energy barrier that is not related to the reaction. It is advisable to compare the optimized reactant and transition state structures of each variant, and to repeat the procedure from different molecular dynamics lengths or velocities before drawing conclusions from small differences. </p>

<br/>
