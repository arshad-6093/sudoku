# Sudoku
 This is a sudoku solver repo in Haskell Programming Language. Initially, the idea was to create an automatic sudoku solver that can solve the sudoku given some elements but ended up implementing both auto solver and interactive solver which interacts with user for the fields and value which the user need to enter the opt element. Some of the considerations made are as follow,
  * Considering sudokuTable as a tuple of 3 Int. First int denotes the value, and next two tuples as rows and columns.
  * Initialising the sudokuTable with 0 in all non-filled rows and columns.
  * Sudoku table is considered as follow
+-------+-------+-------+
| 3 0 5 | 4 0 2 | 0 6 0 |
| 4 9 0 | 7 6 0 | 1 0 8 |
| 6 0 0 | 1 0 3 | 2 4 5 |
+-------+-------+-------+
| 0 0 3 | 9 0 0 | 5 8 0 |
| 9 6 0 | 0 5 8 | 7 0 3 |
| 0 8 1 | 3 0 4 | 0 9 2 |
+-------+-------+-------+
| 0 5 0 | 6 0 1 | 4 0 0 |
| 2 0 0 | 5 4 9 | 0 7 0 |
| 1 4 9 | 0 0 7 | 3 0 6 |
+-------+-------+-------+
0 represents the empty field. Input to this solver should be in this format.
