 def decisionTree(i):
     return i.tree(i.rows,
                  y   = lambda z: z.cells[i.cols.klass.pos],
                  yis = Sym)
  def regressionTree(i):
     return i.tree(i.rows,
                  y   = lambda z: last(z.cells),
                  yis = Num)
Here's my tree learner.

I don't sort all the attributes and their cuts (too much memory). Instead, I just keep the best one seen so far
When splitting, I call tree recursively to build the kids
And for that recursion, I iterate over all the cuts found in my best column
  def tree(i,lst,y,yis,lvl=0):
    if len(lst) >= THE.tree.minObs*2:
      # find the best column
      lo, cut, col = 10**32, None, None
      for col1 in i.cols.indep:
        x = lambda row: row.cells[col1.pos]
        cut1, lo1 = col1.div(lst, x=x, y=y, yis=yis)
        if cut1:
          if lo1 < lo:
            cut, lo, col = cut1, lo1, col1
      # if a cut exists
      if cut:
        # split data on best col, call i.tree on each split
        x = lambda row: row.cells[col.pos]
        return [o(lo   = lo,
                  hi   = hi,
                  n    = len(kids),
                  txt  = col.txt,
                  kids = i.tree(kids,y,yis,lvl+1)
                ) for lo,hi,kids in col.split(lst, x, cut)]
    return yis(lst,key=y)
Debugging tip #1

See the lvl argument in tree?
incremented on each recursive call
If used in a print statement, you can watch your tree growth working, or growing out of control.
Debugging tip #2:

Write a tree print utility FIRST since you will be using it all the time during debugging.
def showt(tree,pre= '',rnd=THE.tree.rnd):
  most = sorted(x.n for x in tree)[-1]
  for x  in tree:
    after =""
    s = x.txt + ' = ' + str(x.lo)
    if x.n == most:
      after,most = "*", None
    if x.lo != x.hi:
      s += ' .. ' + str(x.hi)
    if isa(x.kids,Thing):
       print(pre + s,after,
             ":",x.kids.middle(rnd),
            '('+str(x.kids.n) +')')
    else:
       print(pre + s,after)
       showt(x.kids,pre + '|   ')
