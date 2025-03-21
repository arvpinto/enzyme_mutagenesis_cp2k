<br>

<h2><p align="center"> <b>Mutagenesis Scan</b> </p> </h2>

<p align="justify"><b>This protocol is designed to perform a mutagenesis scan with residues such as ALA or GLY. The mutated residues are optimized while keeping the rest of the system fixed, and then single-point calculations are performed on the provided reactant and transition state structures. </b></p>

<p align="justify"> It requires a file with a list of residues to be mutated, a *prmtop file, reactant and transition state structures in the *pdb format, LEaP and CP2K input templates and a selection of the QM region. The following packages are also required: CP2K, PyMOL, VMD, LEaP, CPPTRAJ and ParmEd (AmberTools). </p>

---

<br>
<h2> <p align="center"> <b>I - Input Preparation </b> </p></h2>

<br/>

A selection in the format presented in qm_selection.dat is required to adequately build the QM system (the atom numbers will change upon residue deletion, resid's remain the same). The preparation of the selection is explained in the I - Input Preparation section of the <a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/residue_deletion.html" target="_blank">residue deletion protocol</a>.

<br>

The <a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/mutagenesis_scan/mut_scan_qmmm_cp2k.sh" target="_blank">mut_scan_qmmm_cp2k.sh</a> script has the following usage:

<pre style="color: white; background-color: black;">
user@machine:~$ ./mut_scan_qmmm_cp2k.sh &lt;residue_list&gt; &lt;scan_type&gt; &lt;topology&gt; &lt;reactant_structure&gt; &lt;ts_structure&gt; &lt;selection&gt; &lt;leap_template&gt; &lt;cp2k_template&gt; &lt;qm_selection&gt;
</pre>

<p align="justify">It prepares a directory for each residue in the list where the input files for CP2K will be output. The &lt;scan_type&gt; argument represents the three-letter code of the residue used in the scan (ALA and GLY are adequate to evaluate the contribution of sidechains, while larger residues might not optimize to an adequate pose). The LEaP input should be consistent with the original parameterization. To obtain the structures and topology of the mutated enzyme, the script calls the sp_mutation.sh script, which has the following usage:</p>

<pre style="color: white; background-color: black;">
user@machine:~$ ./sp_mutation.sh &lt;number&gt; &lt;residue&gt; &lt;topology&gt; &lt;reactant_structure&gt; &lt;ts_structure&gt; &lt;selection&gt; &lt;leap_template&gt;
</pre>

<p align="justify">Since mutating residues changes the atom numbering, the QM/MM settings must be updated for each mutation. The <a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/mutagenesis_scan/mut_qm_sel.sh" target="_blank">mut_qm_sel.sh</a> script checks how the mutated residue should be included in the QM layer and modifies the qm_selection.dat file accordingly. It has the following usage:</p>

<pre style="color: white; background-color: black;">
user@machine:~$ ./mut_qm_sel.sh &lt;number&gt; &lt;residue&gt; &lt;topology&gt; &lt;qm_selection&gt;
</pre>
    
<p align="justify"><a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/mutagenesis_scan/vmd_forceeval.tcl" target="_blank">vmd_forceeval.tcl</a> script is called within the latter to produce a file with the configuration of the QM layer, defined by the selection in the qm_selection.dat file. The cp2k_template.inp file is used to produce geometry optimization and a single-point point energy input files.</p>

<br/>

The calculations can then be run using a for loop:
<pre style="color: white; background-color: black;">
user@machine:~$ for i in ALA_*; do cd "$i" ; cp2k.popt -i opt_res_R.inp -o opt_res_R.out ; cp2k.popt -i scan_res_R.inp -o scan_res_R.out ; cp2k.popt -i opt_res_TS.inp -o opt_res_TS.out ; cp2k.popt -i scan_res_TS.inp -o scan_res_TS.out ; cd .. ; done
</pre>

<div align="center">
    <img src="assets/img/mut_multi_md.gif" width="500">
</div>

<br/>

<h2> <p align="center"> <b>II - Output Processing</b> </p></h2>

<br>

After running the optimization and single-point calculations, the following command allows us to extract the absolute energies and calculate the R->TS energy barrier for each residue mutation:

<pre style="color: white; background-color: black;">
user@machine:~$ paste <(for i in ALA_*; do echo "$i" | sed 's/ALA_//g'; done) <(for i in ALA_*; do echo $(grep "Total FORCE" "$i"/scan_res_TS.out | tail -n -1) ; done | awk '{print $9}') <(for i in ALA_*; do echo $(grep "Total FORCE" "$i"/scan_res_R.out | tail -n -1) ; done | awk '{print $9}') | awk '{print $1,($2-$3)*627.509}' | sort -n -k1,1 > energy_differences_mut.dat
</pre>

<br/>

<p align="justify">The energy barriers can be compared to the ones calculated with the <a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/residue_deletion.html" target="_blank">residue deletion protocol</a> using the <a href="https://arvpinto.github.io/enzyme_mutagenesis_cp2k/mutagenesis_scan/E_diff_comparison_bar_plot.py" target="_blank">E_diff_comparison_bar_plot.py</a> script:</p>

<pre style="color: white; background-color: black;">
user@machine:~$ python E_diff_comparison_bar_plot.py energy_differences_del.dat energy_differences_mut.dat
</pre>

<br>

<div align="center">
    <img src="mutagenesis_scan/comparison_bar_plot.png">
</div>

<br/>

<p align="justify"> The calculated energy barriers upon deletion can be compared with the original energy barrier (14.8 kcal⋅mol<sup>-1</sup>) to see if the residues are stabilizing or destabilizing to the transition state of the reaction step. Here, we can see that the deletion of most residues is unfavorable (this is expected for a small protein in a system with a considerable amount of charged molecules), however, the deletion of residue 92 decreases the energy barrier. </p>

<br>

<p align="justify"> The script plots the calculated energy barriers against d<sub>1</sub>-d<sub>2</sub>, where d<sub>1</sub> is the distance between the center of geometry of the deleted residue and the midpoint between the atoms that represent the direction of the positive charge, and d<sub>2</sub> is the distance between the center of geometry of the deleted residue and the midpoint between the atoms that represent the direction of the negative charge. Suppose the used conformation is considerably stable and the charge separation reaction is simple. In that case, this approach can accurately capture and quantify the electrostatic contribution of residues to the transition state. Usually, negatively charged residues close to the positive moiety stabilize the transition state, while positively charged residues are destabilizing (and vice-versa relative to the negatively charged moiety). Note that the electric field induced by enzymes can be quite complex and the latter assumption might not always be true. For example, if a positively charged residue is close to the positive charge, but stabilizing the negative charges of two chemical groups nearby, the repulsion that arises from the lack of neutralizing positive charge may increase the energy barrier instead. In any case, when discussing this type of result, the environment of the specific residues should be taken into account. </p>

<br/>
