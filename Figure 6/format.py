import csv
            
from Bio import SeqIO

#fix numbering issue due to deletion of N-terminal
with open ("mistic.csv","w") as out:
    for r in csv.reader(open("MI_mfDCA_plmDCA_gaussianDCA_links.csv","r")):
        if "i" not in r[0]:
            new_line = r[:2]+[int(r[0])+47]+[int(r[1])+47]+r[2:]
            print (",".join(map(str,new_line)),file=out)
        else:
            new_line = r[:2]+["i-mouse,j-mouse"]+r[2:]
            print (",".join(map(str,new_line)),file=out)


#add conservation score from consurf

conservation = {}

for r in csv.reader(open("consurf.csv","r")):
    conservation[r[0]] = r[2:]

with open("mistic-consurf.csv","w") as out:
    for r in csv.reader(open("mistic.csv","r")):
        if "i-mouse" not in r[2]:
            print (",".join(r+[conservation[r[2]][0], conservation[r[3]][0],conservation[r[2]][4], conservation[r[2]][5],conservation[r[3]][4],conservation[r[3]][5]]),file=out)
        else:
            print (",".join(r+["i-conservation","j-conservation","i-b/e","i-f/s","j-b/e","j-f/s"]),file=out)
            
#add residue for easier analysis

for r in SeqIO.parse("Mus_musculus.fas","fasta"):
    mouse = str(r.seq)

with open("mistic-consurf-residues.csv","w") as out:
    for r in csv.reader( open("mistic-consurf.csv","r") ):
        if "i-mouse" not in r[2]:
            i =  int(r[2])-1
            j = int(r[3])-1
            print (",".join((r[0:3]+[mouse[i]]+[r[3]]+[mouse[j]]+r[4:])), file=out)