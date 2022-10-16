import json

var_names = {}

with open('datasets/list_atributes.txt') as f:
    lines = f.readlines()


for line in lines[1:]:
    if len(line)>1:
        if line[1]!=':' and (line[1] not in ['<', '>']) and ('Missing' not in line) and ('Cases Fraction' not in line) and ('.' in line) and ('%' not in line):
            var_type=line.split(':')
            #print(var_type[0])
            #print(var_type[-1])
            var_names[var_type[0]]=var_type[-1][:-1]

with open('datasets/var_type.txt', 'w') as file:
     file.write(json.dumps(var_names)) # use `json.loads` to do the reverse
