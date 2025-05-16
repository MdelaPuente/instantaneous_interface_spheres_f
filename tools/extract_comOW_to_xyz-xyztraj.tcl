package require  pbctools

# READ XYZ FILE: change name if needed!
mol new test_xyz.xyz type xyz first 0 last -1 step 1 filebonds 1 autobonds 1 waitfor all

# Define output file and remove it if it exists already
file delete -force COM_OW.xyz
set outfile [open "COM_OW.xyz" w]

# Select water oxygen atoms (adapt name if needed!)
set OW [atomselect top "name OW"]

# Get number of frames
set num_frames [molinfo top get numframes]

# Loop over trajectory and save COM as xyz
for {set i 0} {$i < $num_frames} {incr i} {
    # Load OW selection in i-th frame
    $OW frame $i

    # Compute com (no need of weight if identical atoms!)
    set com [measure center $OW] 
    
    # Write XYZ frame
    puts $outfile "1"
    puts $outfile "Frame $i - COM of water oxygen atoms"
    puts $outfile "COM [lindex $com 0] [lindex $com 1] [lindex $com 2]"
}

# Exit VMD
quit
