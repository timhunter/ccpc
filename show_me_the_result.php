<?php 
/* show_me_the_result.php */
/* by Jiwon Yun */
/* It creates html files to show the ER changes */

$language_array = array("chinese","japanese","korean");
$grammar_type_array = array("promotion","adjunction","pronominal");
$prefix_label_array = array("SRC","ORC");
$prefix_array_array = array(
	"chinese"=>array(
		"SRC"=>array("Vt","Noun","de","Noun"), 
		"ORC"=>array("Noun","Vt","de","Noun")),
	"japanese"=>array(
		"SRC"=>array("N","o","Vt","N"), 
		"ORC"=>array("N","ga","Vt","N")),
	"korean"=>array(
		"SRC"=>array("N","acc","Vtn","N"), 
		"ORC"=>array("N","nom","Vtn","N"))
);

$index_file_name = "index.html";
$index_file = fopen($index_file_name, 'w');
$html_text = 
	"<HTML>\n".
	"<HEAD>\n".
	"</HEAD>\n\n".
	"<BODY>\n";
fwrite($index_file, $html_text);

foreach ($language_array as $language)
{
	foreach ($grammar_type_array as $grammar_type)
	{
		$command = "./getwmcfg.csh ".$language." ".$grammar_type;
		system($command);
		
		$grammar = $language.$grammar_type;

		$html_file_name = $grammar.".html";
		$html_file = fopen($html_file_name, 'w');
		$html_text = 
			"<HTML>\n".
			"<HEAD>\n".
			"</HEAD>\n\n".
			"<BODY>\n";
		fwrite($html_file, $html_text);

		$html_text = "<a href=\"".$html_file_name."\">".$language." ".$grammar_type."</a><BR>"
		fwrite($index_file, $html_text);
	
		foreach($prefix_label_array as $prefix_label)
		{
			$prefix_array = $prefix_array_array[$language][$prefix_label];
			$prefix_string = implode(" ",array_slice($prefix_array,0,sizeof($prefix_array)));
			$html_text = 
				"<h2>".$prefix_label." \"".$prefix_string."\"</h2>\n".
				"<TABLE>\n";
			fwrite($html_file, $html_text);
	
			$entropy_array = array();
			$entropy_array[] = 0; // initialize
			$html_text_entropy = "	<TR>\n";
			$html_text_ER = "	<TR>\n";
			$html_text_topstring = "	<TR>\n";
	
			for ($i=0;$i<sizeof($prefix_array);$i++)
			{
/* processing one prefix */ 

/* get the result */
$prefix_string = implode(" ",array_slice($prefix_array,0,$i+1));
$prefix_string_without_space = implode("-",array_slice($prefix_array,0,$i+1));
echo "Result for ".$prefix_string."\n";

$topstring_file_name = "topstring.".$grammar.".".$prefix_string_without_space.".txt";
$command = "./topstrings2.csh ".$grammar." \"".$prefix_string."\" > ".$topstring_file_name;

system($command);	

/* show the result */

/* entropy */

$chart_file_name = $grammar.".".$prefix_string_without_space.".global.chart";
$chart_file = fopen($chart_file_name, 'r');

while (!feof($chart_file))
{
	$line = fgets($chart_file);
	if (preg_match("/entropy/",$line))
	{
		$line = str_replace("(* \"entropy = ", "", $line);
		$line = str_replace("\" *)", "", $line);
//		$line = round($line,3);
		echo $line."\n";
		$entropy_array[] = $line;
		$ER = $entropy_array[$i] - $line;
		if ($ER<0)
		{
			$ER = 0;
		}

		$pdf_file_name = $grammar.".".$prefix_string_without_space.".global.pdf";
		$html_text_entropy = $html_text_entropy.
			"		<TD width=300><a href=\"".$pdf_file_name."\">".$line."</a></TD>\n";
		$html_text_ER = $html_text_ER.
			"		<TD><font color=#cc0000>".$ER."</font></TD>\n";
		break;
	}
}

/* topstring */

$topstring_file = fopen($topstring_file_name, 'r');

$html_text_topstring = $html_text_topstring."		<TD>";
for ($j=0;($j<10)&&(!feof($topstring_file));$j++)
{
	$line = fgets($topstring_file);
	$temp_array = explode(" ",$line);
	$temp_array[0] = number_format($temp_array[0],3);
	$temp_array[0] = "<font color=#0099ff>".$temp_array[0]."</font>";
	$line = implode(" ",$temp_array);
	$html_text_topstring = $html_text_topstring.$line."<BR>";
}
$html_text_topstring = $html_text_topstring.$line."</TD>\n";

fclose($topstring_file);
fclose($chart_file);

/* end of processing one prefix */

		} // end of each prefix
	
			$html_text_entropy = $html_text_entropy."	</TR>\n";
			$html_text_ER = $html_text_ER."	</TR>\n";
			$html_text_topstring = $html_text_topstring."	</TR>\n";
	
			fwrite($html_file, $html_text_entropy);
			fwrite($html_file, $html_text_ER);
			fwrite($html_file, $html_text_topstring);
	
			$html_text = 
				"</TABLE>\n";
			fwrite($html_file, $html_text);
		} // end of each sentence
	
		$html_text = 
			"</BODY>\n".
			"</HTML>\n";
		fwrite($html_file, $html_text);
		fclose($html_file);
	} // end of each grammar
} // end of each language

$html_text = 
	"</BODY>\n".
	"</HTML>\n";
fwrite($index_file, $html_text);
fclose($index_file);

system("rm topstring.*.txt");
system("rm *.chart");
system("mv *.html result");
system("mv *.pdf result");

?>
