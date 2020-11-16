<?php

session_start();

if (!isset($_SERVER['QUERY_STRING']) || "${_SERVER['QUERY_STRING']}" == "")
  $self = "${_SERVER['PHP_SELF']}";
else
  $self = "${_SERVER['PHP_SELF']}?${_SERVER['QUERY_STRING']}";

if (isset($_GET['cheat']))
  $cheat = true;
else
  $cheat = false;

if (isset($_GET['limit']))
  $limit = $_GET['limit'];
else
  $limit = 10;

function microtime_float()
{
  list($usec, $sec) = explode(" ", microtime());
  return ((float)$usec + (float)$sec);
}

function prime($p) {
  if ($p == 2)
    return true;
  if ($p % 2 == 0)
    return false;
  for ($i = 3; $i * $i <= $p; $i += 2)
    if ($p % $i == 0)
      return false;
  return true;  
}

function mod_inv($n, $p){
	$t = 0; $nt = 1; $r = $p; $nr = $n % $p;
	while ($nr != 0) {
		$quot= intdiv($r, $nr);
		$tmp = $nt;  $nt = $t - $quot*$nt;  $t = $tmp;
		$tmp = $nr;  $nr = $r - $quot*$nr;  $r = $tmp;
	}
	if ($t < 0) $t += $p;
	return $t;
}

function binomial($n, $k, $p) {
  $min = min($k, $n - $k);
  $num = 1;
  $denom = 1;
  for ($i = 1; $i <= $min; $i++) {
  	$denom = ($denom * $i) % $p;
  	$num = ($num * ($n + 1 - $i)) % $p;
  }
  return ($num * mod_inv($denom, $p)) % $p;
}

function generate() {
  $_SESSION['count']++;

  if ($_SESSION['count'] <= 3)
    $upper = 100;
  else
  	$upper = pow(10, $_SESSION['count'] - 1);
  
  if ($_SESSION['count'] == 1)
  	$lower = 0;
  else
  	$lower = $upper/10;

  do {
  	$p = rand($lower, $upper);
    if ($p % 2 == 0)
      $p++;    
    while ($p < $upper)
      if (prime($p))
        break;
      else
        $p += 2;
  } while ($p >= $upper);

  $n = rand($lower, $p - 1);
  $k = rand($lower, $n);
  
  $_SESSION['p'] = $p;
  $_SESSION['n'] = $n;
  $_SESSION['k'] = $k;

  $_SESSION['answer'] = binomial($n, $k, $p);
}

?>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml"><head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>Count combinations in modular arithmetic!</title>
<link href="combinations.css" rel="stylesheet" type="text/css" />

<?php
if (!isset($_SESSION['count']) || isset($_SESSION['reset'])) {
  $_SESSION['count'] = 0;
  $_SESSION['wrong'] = 0;
  $_SESSION['start_time'] = microtime_float();
}

if (isset($_SESSION['generate']) || $_SESSION['count'] == 0)
  generate();

unset($_SESSION['generate']);
unset($_SESSION['reset']);
?>

<h1>Count combinations in modular arithmetic!</h1>
<p>In how many ways can we choose <span class="emph">K</span> elements out
  of a set of <span class="emph">N</span> elements, if their order is not
  important?  Obviously, the answer is <span class="emph">C(N, K)</span>,
  in other words,
  a <a href="https://en.wikipedia.org/wiki/Binomial_coefficient">binomial
  coefficient</a>.
</p>
<p>This number may be very large and, for this reason, we want to calculate
  the remainder (modulo) of its division by a large prime number
  <span class="emph">P</span>.
</p><p>
</p><p>Assume that <span class="emph">0 ≤ K ≤ N &lt; P ≤ 10<sup>9</sup></span>
  and that <span class="emph">P</span> is a prime number.</p>
<blockquote>
  <p>For example, if <span class="emph">N = 42</span>,
    <span class="emph">K = 17</span> and
    <span class="emph">P = 690419</span>,
    the result is <span class="emph">188587</span>.
  </p>
  <p>In fact, <span class="emph">C(42, 17) = 254661927156 =
    368851 × 690419 + 188587</span>.
  </p>
</blockquote>

<hr>

<p><span class="question">Question <?= "${_SESSION['count']}"; ?></span>
  </p><p>What is the value of
  <span class="emph">C(<span id="N"><?= "${_SESSION['n']}"; ?></span>,
    <span id="K"><?= "${_SESSION['k']}"; ?></span>) modulo
    <span id="P"><?= "${_SESSION['p']}"?></span></span>?
    <?php
    if ($cheat)
    	echo "<p>Hmmm, how about " . $_SESSION['answer'] . "?</p>"
    ?>
</p>

<?php
if (isset($_POST['answer'])) {
  if ($_POST['answer'] == $_SESSION['answer']) {
    echo "<p class=\"right\">RIGHT! :)</p>\n";
    if ($_SESSION['count'] < $limit) {
      $_SESSION['generate'] = true;
      echo "<form action=" . htmlentities($self) . " id=\"cr\" name=\"cr\" method=\"post\">";
      echo "<td><input type=\"submit\" class=\"button\" name=\"continue\" value=\"Continue!\" autofocus=\"\"></td>\n</form>";
    }
    else
      $_SESSION['reset'] = true;
  }
  else {
    echo "<p class=\"wrong\">WRONG</p>\n";
    $_SESSION['wrong']++;
    echo "<form action=" . htmlentities($self) . " id=\"cw\" name=\"cw\" method=\"post\">";
    echo "<td><input type=\"submit\" class=\"button\" name=\"continue\" value=\"Continue!\" autofocus=\"\"></td>\n</form>";
  }
}
else { ?>
	<form action="<?php echo htmlentities($self); ?>" id="sub" name="sub" method="post">
	<table cellspacing="3" border="0">
	<tbody><tr>
	  <td><input type="text" class="box" name="answer" autofocus=""></td>
	  <td><input type="submit" class="button" name="submit" value="Submit!"></td>
	</tr>
	</tbody></table>
	</form>
			 <?php 
} ?>


<?php
  if (isset($_SESSION['reset'])) {
?>
<p><span class="congratulations">Congratulations!</span>
   You answered all questions!</p>
<p>It took you
  <?php printf("%0.1lf", microtime_float() - $_SESSION['start_time']); ?> seconds
<?php
  if ($_SESSION['wrong'] == 0) {
?>
  and you made no mistakes.
<?php
  }
  else if ($_SESSION['wrong'] == 1) {
?>
  and you made one mistake.
<?php
  }
  else {
?>
  and you made <?php echo "${_SESSION['wrong']}" ?> mistakes.</p>
<?php
  }
?>
<form action="<?= htmlentities($self); ?>" id="again" name="again" method="post">
<td><input type="submit" class="button" name="again" value="Play again!" autofocus="" /></td>
</form>
<?php
  }
?>

</body>

<div id="footer">
    <h3 class="rfooter">HTML and CSS by <a href="https://courses.softlab.ntua.gr/pl2/2019b/exercises/combmod.php">combmod</a> and PHP based on <a href="https://courses.softlab.ntua.gr/pl2/2010b/examples/scripting2/primality/primality.php">primality</a>.</h3>
    <h3 class="lfooter">You can also cheat and set desired limit (there are 10 questions by default).</h3>

</div>

</html>