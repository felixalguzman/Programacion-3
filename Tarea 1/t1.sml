fun choose(r : int, k : int) = 
    if k = 1 then r
    else if k = orelse k = r then 1
    else choose(r-1,k) + choose(r-1,k-1)
 
fun pascal_triangle(x : int) =
    if (x <=1) then [[1]]
    else
        let fun count (from:int) =
            if from=x then 1::[] else choose(x,from) :: count(from+1)            
        in count()::pascal_triangle(x-1)
    end