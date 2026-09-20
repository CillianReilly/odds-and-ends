// sudoku solver
// https://cillianreilly.com/blog/sudoku-solver.q
p,:3 sv div[;3]p:9 vs til 81
f:{[p;t;x;y]@[x;y;:;]each t except x*any p=p[;y]}[p;til 10]
s:{{raze f'[x;y]}/[enlist x;where not x]}

// s <vector of flattened 9x9 grid>
