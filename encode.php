<?php
$deck=json_decode($_GET['json']); // expects [{"qty": (int), "cardid": (string)},{...}]
if (!is_array($deck) || json_last_error() != JSON_ERROR_NONE) die("Bad JSON - ".(json_last_error() == JSON_ERROR_NONE?"JSON is not an array":json_last_error_msg()));
$sixtyfour=array("A","B","C","D","E","","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","0","1","2","3","4","5","6","7","8","9","-","_");
$rarities=array("S","C","U","R","P","XR","UR");
$sets=array("JL","GL","AT");
$version=1;

$output=$sixtyfour[$version-1];                                                          // version number is 6 bits

foreach ($deck as $card){
if(!preg_match("/^(C|U|R|UR|XR|S|P){1}(\d*)\-(JL|GL|AT){1}$/",$card->cardid,$matches)) die("Malformed JSON - no match for ".$card->cardid);
// $matches[1] is rarity (string), [2] is card number, and [3] is set (string)

if($card->qty < 1 || $card->qty > 4) die("Bad (or missing) 'qty' for ".$card->cardid);
if($matches[2] > 256) die("Card number out of range for ".$card->cardid);

$qty=    str_pad(decbin($card->qty-1),2,"0",STR_PAD_LEFT);                                // qty is 2 bits
$rarity= str_pad(decbin(intval(array_search($matches[1],$rarities))),3,"0",STR_PAD_LEFT); // rarity (enum) is 3 bits
$cardnum=str_pad(decbin($matches[2]-1),8,"0",STR_PAD_LEFT);                               // card number is 8 bits
$set=    str_pad(decbin(intval(array_search($matches[3],$sets))),4,"0",STR_PAD_LEFT);     // set (enum) is 4 bits
$hash = $qty.$rarity.$cardnum.$set;  //concatenate them all into a big binary string
$checkbit = substr_count($hash,"1") % 2; // calculate even parity by counting the 1s      // parity bit is 1 bit
$output.=$sixtyfour[bindec(substr($hash.$checkbit,0,6))].$sixtyfour[bindec(substr($hash.$checkbit,6,6))].$sixtyfour[bindec(substr($hash.$checkbit,12,6))];
/* the above line converts each chunk of 6 binary digits into decimal and then base64, concatenates it all together into 3 base64 digits
   and then concatenates those 3 digits on to the end of the output string */
}
$checksum=0;
for ($x=0;$x<strlen($output);$x++){// iterate through the base64 string to calc the checksum
        $checksum+=intval(substr($output,$x,1));
}
$checksum=$sixtyfour[$checksum%64];                                                       // checksum is 6 bits
$output.=$checksum;

echo $output;

?>


<?php
$deck=json_decode($_GET['json']);
if (!is_array($deck) || json_last_error() != JSON_ERROR_NONE) die("Bad JSON - ".(json_last_error() == JSON_ERROR_NONE?"JSON is not an array":json_last_error_msg()));
$sixtyfour=array("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","0","1","2","3","4","5","6","7","8","9","-","_");
$rarities=array("S","C","U","R","P","XR","UR");
$sets=array("JL","GL","AT");
$version=1;
$output=$sixtyfour[$version-1];
foreach ($deck as $card){
//echo $card->qty."x ".$card->cardid."<br/>";
if(!preg_match("/^(C|U|R|UR|XR|S|P){1}(\d*)\-(JL|GL|AT){1}$/",$card->cardid,$matches)) die("Malformed JSON - no match for ".$card->cardid);
if($card->qty < 1 || $card->qty > 4) die("Bad (or missing) 'qty' for ".$card->cardid);
if($matches[2] > 256) die("Card number out of range for ".$card->cardid);
$qty=    ($card->qty-1)*65536;                                           // $qty           x65536
$rarity= (array_search($matches[1],$rarities))*8192;                     // $rarity (enum) x8192
$cardnum=($matches[2]-1)*32;                                             // $cardnum       x32
$set=    (array_search($matches[3],$sets))*2;                            // $set (enum)    x2
$hash = $qty+$rarity+$cardnum+$set;                                      // sum them
$checkbit = substr_count(decbin($hash),"1") % 2;                         //$ checkbit      x1
$hash+=$checkbit;                                                        // add checkbit
$sextet3=$hash%64;                                                       // get last 6 bits
$sextet2=(($hash-$sextet3)/64)%64;                                       // get bits 7-12
$sextet1=(($hash-$sextet2-$sextet3)/4096)%64;                            // get bits 13-18
$output.=$sixtyfour[$sextet1].$sixtyfour[$sextet2].$sixtyfour[$sextet3]; // encode and concat
}
$checksum=0;
for ($x=0;$x<strlen($output);$x++){
        $checksum+=intval(substr($output,$x,1));
}
$checksum=$sixtyfour[$checksum%64];
$output.=$checksum;
echo $output;

?>
