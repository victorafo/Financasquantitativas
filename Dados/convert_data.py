import sys
from datetime import datetime

strDate = '2/4/18'

def printfunction(vector):
	lenv = len(vector)
	string=''
	for i in range(0, lenv - 1):
		string += vector[i] + ','
	string += vector[lenv-1]
	return string

print 'Number of arguments:', len(sys.argv), 'arguments.'
print 'Argument List:', str(sys.argv)

f= open(sys.argv[1],"r")
f_out = open(sys.argv[2], "w+")

f1 = f.readlines()

a=[]

for x in f1:
	line = x.replace(", ", " ")
	line = line.split(",")
	line[0] = line[0].replace("\"", "")
	if line[0] != 'Date':
		#print line[0]
		objDate = datetime.strptime(line[0], '%b %d %Y')
		line[0] = datetime.strftime(objDate, '%Y-%d-%m')
		#print line[0]
		#f_out.write(line.toString())
		f_out.write(printfunction(line))
	else:
		print f_out.write(x)
