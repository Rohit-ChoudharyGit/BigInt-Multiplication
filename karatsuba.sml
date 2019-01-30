(* Approach : my approach  is different from the others as using the carry in a list and then adding .First i tried with list method but
 that was taking sloving with errors.
  what I thought is : there is problem with the size of datatype int , if somehow we use some datatype that has no size limit than we are welll with the multiplication using karatsuba
SML provides a datatype intinf.int that has no limit. 
I used the conversion of int values to intinf.int.
 
 *)
exception NotUnsignedIntegers; (*raise exception when string contains anything that is not int *)

(*function size :  helper function to find the length value *)
fun size(num :IntInf.int) =          
    if num<10
    then 1
    else 1 + size(num div 10)
(*function pow: helper function to calculate the power of 10*)		 
fun pow(power: IntInf.int) =
    if power = 0
    then 1
    else LargeInt.fromInt 10 * pow(power -1)

(*function karat : the real karatsuba algorithm *)				  				  
fun karat(x:IntInf.int, y:IntInf.int) =	    
    if(x<10) orelse (y<10) (* if the values some to single digit , so that we can say directly by looking what the multiplication is*)
    then x*y
    else
	let
	    val  n =  size(x) 
	    val  m = LargeInt.fromInt n div 2
	    val  d =  pow(m)				     
            val  x0 =   x mod d (*base 10 is taken in pow function itself*)
	    val  x1 =  x div d
	    val  y0 = y mod d (*base 10 is taken in pow function itself*)
	    val  y1 =  y div d
	    val  z0 = karat(x0,y0)
	    val  z1 = karat((x0 + x1),(y0+y1))
	    val  z2 = karat(x1,y1)
	in
	    z2*pow(2*m)  +(z1-z2-z0)*pow(m)+z0

	end
	    
fun validate(xs:char list) =   (*validate the string for integers only*)
    length (List.filter (Char.isDigit) xs) = length xs;

fun karatsuba(num1:string, num2:string) =  (* main function progrmam*)
    (*valid string - 1. only integers  2. not empty *)
    if not(null( explode(num1)) )andalso not(null (explode(num2))) andalso validate(explode(num1)) andalso validate(explode(num2))			then
	let	
	    val SOME num1a =  LargeInt.fromString(num1)
	    val SOME num2a =  LargeInt.fromString(num2)
	   
			 
	in
            Control.Print.stringDepth := 10000;
	    LargeInt.toString(karat(num1a,num2a)) 	     
	end
    else
	raise NotUnsignedIntegers
	      

	

