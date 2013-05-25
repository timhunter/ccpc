/^[0-9]+ \/ [0-9]+[ \t]+ t33 --> t87/ { $1 = newunary; $3 = total; print ; next }
/^[0-9]+ \/ [0-9]+[ \t]+ t33 --> t2 t0/   {  $1 = newbinary; $3 = total; print ; next}
{ print} 
