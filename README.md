<br>
<p align="justify"><b>These protocols investigate the energetic contribution of enzyme residues to the activation energy using QM/MM approaches within CP2K. The objective is to identify residues that destabilize the transition state, enabling mutations that lower the activation energy and enhance the reaction rate.</b></p>

<p align="justify">Starting from the reactant and transition state of a reaction step, each protocol modifies the enzyme, recalculates the QM/MM energy of both structures and compares the resulting energy barrier with that of the unmodified (WT) system. The three protocols go from the fastest and most approximate to the most realistic:</p>

<ol>
<li><p align="justify"><a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/residue_deletion.html">Residue deletion</a>: residues are removed from the system one at a time, and only single points are calculated;</p></li>
<li><p align="justify"><a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/mutagenesis_scan.html">Mutagenesis scan</a>: residues are mutated to a specified type (e.g., glycine scanning) and the mutated residue is optimized before the single points;</p></li>
<li><p align="justify"><a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/mutagenesis_multi.html">Multipoint mutagenesis</a>: several mutations are introduced at once, followed by classical molecular dynamics and QM/MM geometry optimizations of the region around them.</p></li>
</ol>

<p align="justify">To execute these protocols, the following files are required:</p>

<ul>
<li><p align="justify">PDB structures for both the reactant and transition state geometries of the reaction;</p></li>
<li><p align="justify">the corresponding &#42;prmtop topology file (refer to the <a href="https://arvpinto.github.io/enzyme_neb_cp2k/" target="_blank">Tutorial for Transition State (TS) Searches of Enzymatic Reaction Mechanisms using QM/MM Methods in CP2K</a>).</p></li>
</ul>

<p align="justify">Additional required files can be either obtained through specific instructions or found as sample files in the repository. The protocols' scripts generate input files compatible with CP2K, streamlining the automation of these analyses. The scripts employed in the protocols only require AmberTools (CPPTRAJ, LEaP and ParmEd) and Python 3 with NumPy and Matplotlib, and every script lists its options with <code>-h</code>.</p>

<p align="justify">The protocols can be downloaded from here: <a href="https://github.com/arvpinto/enzyme_mutagenesis_cp2k/archive/refs/heads/main.zip" target="_blank">enzyme_mutagenesis_cp2k</a>. After unzipping the file, run this command to make the scripts available in the current terminal (add it to your ~/.bashrc to make it permanent):</p>

<pre style="color: white; background-color: black;">
<span style="color: #9966CC;">user@machine:~$</span> source enzyme_mutagenesis_cp2k-main/mutagenesis_scripts.sh
</pre>

<p align="center"><b> Once these requirements are fulfilled, you are ready to start with the residue deletion protocol. </b></p>

---

### Example

<p align="justify">The sample files are the reactant and transition state of a reaction step of phospholipid hydrolysis by the human group IIA secreted phospholipase A2 (hGIIA sPLA2), the enzyme of the <a href="https://arvpinto.github.io/enzyme_neb_cp2k/" target="_blank">static QM/MM part of this series</a>. The energy barrier of the unmodified system is 14.55 kcal⋅mol<sup>-1</sup> with the sample settings (CP2K 2024.2). The sample CP2K template is a fast setup meant for testing (PBE functional with a double-zeta basis set), and should be replaced by the level of theory used to obtain the reactant and transition state geometries.</p>

### Files

| Where | What |
|---|---|
| <a href="https://github.com/arvpinto/enzyme_mutagenesis_cp2k/tree/main/hpla2_example" target="_blank">hpla2_example</a> | topology (hpla2_ee.prmtop), reactant and transition state (R.pdb, TS.pdb), CP2K and LEaP templates, QM selection, the QM/MM section of the original calculations (forceeval_qmmm.inc), the residue and variant lists of the three protocols, and the results shown in these pages |
| <a href="https://github.com/arvpinto/enzyme_mutagenesis_cp2k/tree/main/scripts" target="_blank">scripts</a> | the three protocols (del_res_qmmm_cp2k.sh, mut_scan_qmmm_cp2k.sh, mut_mp_qmmm_cp2k.sh), the mutant builder (build_mutant.sh), the QM/MM helper (qmmm_mut_tools.py), the extraction of the energy barriers (extract_barriers.py) and the plotting scripts |
| <a href="https://github.com/arvpinto/enzyme_mutagenesis_cp2k/blob/main/mutagenesis_scripts.sh" target="_blank">mutagenesis_scripts.sh</a> | makes the scripts available in the current terminal |

---

### Publications

<p>Some of these approaches have been employed in:</p>

<ul>
<li><p><a href="https://doi.org/10.1021/acscatal.1c02444" target="_blank">Reaction Mechanism of MHETase, a PET Degrading Enzyme</a></p></li>
<li><p><a href="https://doi.org/10.1039/D4SC02315C" target="_blank">Revisiting the reaction pathways for phospholipid hydrolysis catalyzed by phospholipase A2 with QM/MM methods</a></p></li>
<li><p><a href="https://pubs.acs.org/doi/10.1021/acs.jcim.2c01337" target="_blank">Engineering DszC Mutants from Transition State Macrodipole Considerations and Evolutionary Sequence Analysis</a></p></li>
</ul>
