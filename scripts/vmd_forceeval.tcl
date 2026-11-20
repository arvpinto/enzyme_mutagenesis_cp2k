# vmd_forceeval.tcl
#
# Writes the CP2K QMMM include file (QM cell, QM_KIND and LINK sections) and the sum of the
# MM charges of the QM atoms for the QM layer given on standard input:
#
#   vmd system.prmtop system.pdb -e vmd_forceeval.tcl -dispdev none < qm_selection.dat > vmd.log
#
# Output: forceeval_qmmm.inc and qm_charge.dat (both are only written when everything worked).
# No Tcl package besides VMD itself is needed.

proc qmmm_mean {values} {
    set sum 0.0
    foreach v $values { set sum [expr {$sum + $v}] }
    return [expr {$sum / [llength $values]}]
}

# sample standard deviation (n-1), as in math::statistics::stdev
proc qmmm_stdev {values} {
    set n [llength $values]
    if { $n < 2 } { return 0.0 }
    set mean [qmmm_mean $values]
    set sum 0.0
    foreach v $values { set sum [expr {$sum + ($v - $mean)**2}] }
    return [expr {sqrt($sum / ($n - 1))}]
}

proc qmmm_forceeval {qm_layer} {
    set qmmm_list [list]

    #--> set QM layer
    set qm_layer_sel [atomselect top $qm_layer frame first]
    if { [$qm_layer_sel num] == 0 } {
        error "the QM selection does not match any atom"
    }

    #--> set QM cell (QM atoms plus 5 angstrom on each side)
    lappend qmmm_list "! CP2K > FORCE_EVAL > QMMM > CELL"
    set qmcell_vec [measure minmax $qm_layer_sel]
    set qmcell_size [vecadd [vecsub [lindex $qmcell_vec 1] [lindex $qmcell_vec 0]] {10.0 10.0 10.0}]
    set qmcell_shape [list 90.0 90.0 90.0]
    lappend qmmm_list "&CELL\n  ABC \[angstrom\] [join $qmcell_size "  "]     !size of cell\n  ALPHA_BETA_GAMMA [join $qmcell_shape " "]      !shape of cell\n&END CELL\n"

    #--> set QM atoms
    lappend qmmm_list "! CP2K > FORCE_EVAL > QMMM > QM_KIND"
    set qm_kind [lsort -unique [$qm_layer_sel get element]]
    foreach kind $qm_kind {
        if { $kind eq "X" || $kind eq "" } {
            error "VMD could not assign an element to some QM atoms"
        }
        set kind_sel [atomselect top "element $kind and ($qm_layer)" frame first]
        lappend qmmm_list "&QM_KIND $kind\n  MM_INDEX [$kind_sel get serial]\n&END QM_KIND"
        $kind_sel delete
    }

    #--> set link atoms
    lappend qmmm_list "! CP2K > FORCE_EVAL > QMMM > LINK"
    set qm_index [$qm_layer_sel get index]
    set heavy_sel [atomselect top "noh ($qm_layer)" frame first]
    set heavy_qm_index [$heavy_sel get index]
    $heavy_sel delete
    foreach id $heavy_qm_index {
        set id_sel [atomselect top "index $id"]
        set resi [$id_sel get resid]
        foreach conect [lindex [$id_sel getbonds] 0] {
            if { [lsearch -exact $qm_index $conect] == -1 } {
                set link_sel [atomselect top "index $conect"]
                set res_sel [atomselect top "same residue as index $id"]
                set q_link [$link_sel get charge]
                set charges [$res_sel get charge]
                set mean_q [qmmm_mean $charges]
                set stdev_q [qmmm_stdev $charges]
                if { $stdev_q > 0.0 } {
                    set qmmm_sc [expr {0.83*exp(-0.5*(abs(($q_link-$mean_q)/$stdev_q)**2))}]
                } else {
                    set qmmm_sc 0.83
                }
                set alpha_imomm [expr {1.38+0.3*(0.83-$qmmm_sc)**2}]
                set types "[$id_sel get type]-[$link_sel get type]"
                lappend qmmm_list "&LINK\n  ALPHA_IMOMM [format "%.2f" $alpha_imomm]     !$types bond (default is ALPHA_IMOMM 1.38 for C-C bond)\n  LINK_TYPE IMOMM\n  MM_INDEX  [expr {$conect+1}]\n  QM_INDEX  [expr {$id+1}]\n  QMMM_SCALE_FACTOR [format "%.1f" $qmmm_sc]\n&END LINK"
                $link_sel delete
                $res_sel delete
            }
        }
        $id_sel delete
    }

    #--> calculate charge
    set qm_q [expr [join [$qm_layer_sel get charge] +]]
    $qm_layer_sel delete

    #--> write INCLUDE file for CP2K QMMM section
    set cp2k_inc [open forceeval_qmmm.inc w]
    puts $cp2k_inc [join $qmmm_list "\n"]
    close $cp2k_inc

    set chargefile [open qm_charge.dat w]
    puts $chargefile "$qm_q"
    close $chargefile
}

file delete forceeval_qmmm.inc qm_charge.dat
set qm_layer [string trim [gets stdin]]
if { [catch { qmmm_forceeval $qm_layer } qmmm_error] } {
    puts "ERROR in vmd_forceeval.tcl: $qmmm_error"
    file delete forceeval_qmmm.inc qm_charge.dat
} else {
    puts "vmd_forceeval.tcl finished normally"
}
quit
