import os
for i in range(11):
    print("Test", i)
    os.system("stack run < examples/good%s.in > outs/%s.ps" % (str(i).zfill(2), i))
    os.system("diff examples/good%s.ps outs/%s.ps" % (str(i).zfill(2), i))
for i in range(1, 4):
    print("Bad test", i)
    os.system("stack run < examples/bad%s.in > outs/bad%s.ps" % (str(i).zfill(2), i))
    os.system("diff outs/errout.ps outs/bad%s.ps" % i)
