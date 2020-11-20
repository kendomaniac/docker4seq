#!/usr/bin/env python3
import sys 
import os 

inputfilename = sys.argv[1]
if not os.path.isfile(inputfilename):
    print("Error! File {0} not found".format(inputfilename))
    raise SystemExit()


outputfilename = '{0}_formatted.fastq'.format('.'.join(inputfilename.split('.')[:-1]))

with open(inputfilename, 'r') as f:
    lines = f.readlines()

count=0
pattern= []
for line in lines:
    line = line.strip('\n')
    u = ''.join(line.split())
    if count == 0:
        if len(u) < 76:
            pattern.append('@'+u+'\n')
            count = count+ 1
        else:
            count =0
    elif count == 1:
        if len(u) == 76:
            pattern.append(u+'\n')
            count = count+ 1
        else:
            count = 0
    elif count == 2:
        if len(u) ==0:
            pattern.append("+"+'\n')
            count = count+ 1
        else:
            count =0
    elif count == 3:
        if len(u) == 76:
            pattern.append(u+'\n')
            count = 0
        else:
            count = 0
            



with open(outputfilename, 'w+') as f:
    for item in pattern:
        f.write("%s" % item)
        
