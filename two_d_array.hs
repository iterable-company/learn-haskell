addThreeElementsOffset arr offset = sum(take 3 (drop offset arr))
extractRow arr row = arr !! row
firstRow arr row col = addThreeElementsOffset (extractRow arr row) col
secondRow arr row col = (extractRow arr (row+1)) !! (col + 1)
thirdRow arr row col = addThreeElementsOffset (extractRow arr (row+2)) col

solve arr = [sum(take 3 (drop col (arr !! row))) + (arr !! (row+1) !! (col + 1)) + sum(take 3 (drop col (arr !! (row + 2)))) | row <- [0..3], col <- [0..3]]
