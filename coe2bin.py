# EP convert .coe file to binary
# started 2019-03-13
#
import sys
import os.path

fin = open(sys.argv[1], 'r')
fout = open(sys.argv[2], 'wb')

data = fin.readlines()
fin.close()

# strip whitespace
data = [x.strip() for x in data]

print '{} lines of input'.format(len(data))
bin_bytes=[]
for i in range(2, len(data)):
    line = data[i]
    comma = line.find(',')
    hd = line[0:comma]
    # print '{line:d} {digit} '.format(line=i, digit=hd)
    bin_bytes.append(int(hd,16))
    # fout.write(int(hd,16))

print '{} bytes written to {}'.format(len(bin_bytes), sys.argv[2] )
fout.write(bytearray(bin_bytes))

fout.close()
