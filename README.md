<p align="justify"><b>These protocols explore the energetic contribution of enzyme residues to the activation energy using QM/MM approaches within CP2K. The goal is to search for residues that destabilize the transition state and mutate them to lower the activation energy and increase the reaction rate.</b></p>

<p align="justify"> The protocols consist of residue deletion (where residues are completely removed from the system), mutagenesis scan (where residues are mutated to a specific residue and their geometry is optimized, e.g. alanine scanning), and multipoint mutant mutagenesis (where several mutations may be introduced and they are submitted to molecular dynamics and geometry optimization procedures). The protocols require the PDB structures of the reactant and transition state geometries of a given reaction and the respective *prmtop topology file (see <a href="https://arvpinto.github.io/enzyme_neb_cp2k" target="_blank">Tutorial for Transition State (TS) searches of Enzymatic Reaction Mechanisms using QM/MM methods in CP2K</a>). Other files are also required but there's an explanation on how to obtain them or they are given as samples in the repository. The scripts employed in the protocols require the availability of software such as VMD, PyMOL, Ambertools and ParmEd. The scripts produce input files that can be directly run in CP2K, thus providing pipelines to automatized these analyses.

The protocols can be downloaded from here <a href="https://github.com/arvpinto/test/archive/refs/heads/main.zip" target="_blank">enzyme_mutagenesis_cp2k-main.zip</a>, and the included scripts have to be given executable permissions with 'chmod +x'.

Some of these approaches have been employed in <a href="https://doi.org/10.1021/acscatal.1c02444" target="_blank">Reaction Mechanism of MHETase, a PET Degrading Enzyme</a>, <a href="https://doi.org/10.1039/D4SC02315C" target="_blank">Revisiting the reaction pathways for phospholipid hydrolysis catalyzed by phospholipase A2 with QM/MM methods</a> and <a href="https://pubs.acs.org/doi/10.1021/acs.jcim.2c01337" target="_blank">Engineering DszC Mutants from Transition State Macrodipole Considerations and Evolutionary Sequence Analysis</a>.</p>










