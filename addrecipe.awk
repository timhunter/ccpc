$1 ~ /Preterminal/ { print $0 }

$1 ~ /Unary/       { print $0,"[0,0]" }

$1 ~ /Binary/	    { print  $0,"[0,0][1,0]" }
