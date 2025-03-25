<p align="justify"><b>These protocols investigate the energetic contribution of enzyme residues to the activation energy using QM/MM approaches within CP2K. The objective is to identify residues that destabilize the transition state, enabling mutations that lower the activation energy and enhance the reaction rate.</b></p>

<br>

<p><b>The protocols include:</b>
<br>
● Residue Deletion: Residues are completely removed from the system
<br>
● Mutagenesis Scan: Residues are mutated to a specified type (e.g., glycine scanning), and their geometry is optimized
<br>
● Multipoint Mutant Mutagenesis: Multiple mutations are introduced, followed by molecular dynamics and geometry optimization procedures
<br>
<br>
<b>To execute these protocols, the following files are required:</b>
<br>
● PDB structures for both the reactant and transition state geometries of the reaction
<br>
● The corresponding *prmtop topology file (refer to the Tutorial for Transition State (TS) Searches of Enzymatic Reaction Mechanisms using QM/MM Methods in CP2K)
</p>

<p align="justify"> Additional required files can be either obtained through specific instructions or found as sample files in the repository. The protocols’ scripts generate input files compatible with CP2K, streamlining the automation of these analyses. The scripts employed in the protocols require the availability of software such as VMD, PyMOL, Ambertools and ParmEd. The scripts produce input files that can be directly run in CP2K, thus providing pipelines to automatized these analyses.
<br>
<br>
The protocols can be downloaded from here <a href="https://github.com/arvpinto/enzyme_mutagenesis_cp2k/archive/refs/heads/main.zip" target="_blank">enzyme_mutagenesis_cp2k</a>.
</p>

---

<br>
<p>Some of these approaches have been employed in:</p>

<br/>

<p><a href="https://doi.org/10.1021/acscatal.1c02444" target="_blank">● Reaction Mechanism of MHETase, a PET Degrading Enzyme</a></p>
<p><a href="https://doi.org/10.1039/D4SC02315C" target="_blank">● Revisiting the reaction pathways for phospholipid hydrolysis catalyzed by phospholipase A2 with QM/MM methods</a></p>
<p><a href="https://pubs.acs.org/doi/10.1021/acs.jcim.2c01337" target="_blank">● Engineering DszC Mutants from Transition State Macrodipole Considerations and Evolutionary Sequence Analysis</a></p>
<br>






