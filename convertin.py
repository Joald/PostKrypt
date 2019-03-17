import sys
prologue = "300 400 translate\n"

epilogue = "stroke showpage"
fname = sys.argv[1]
path = fname.split('/')
outname = path[-1][:-3]
outname = "out%s.ps" % outname
path[-1] = outname
outname = '/'.join(path)
with open(outname, "w") as outf, open(fname, "r") as inf:
    outf.write(prologue + inf.read() + epilogue)
