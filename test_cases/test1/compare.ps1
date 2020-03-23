

$InputFile = 'labeltest.asm'
$File1 = 'expected'
$File2 = 'labeltest'

Write-Output $Compiler
$Options = '-n', '-m'
$Extensions = '.nes', '.nes.ram.nl', '.mlb'

$out1 = asm6f_64 $InputFile "$File1.nes" $Options
$out2 = C:\Users\JFt36\Dropbox_old\Coding\VSCode_Extention\asm6f\asm6f++.exe $InputFile "$File2.nes" $Options

Write-Output $out1
Write-Output $out2


foreach ($Extension in $Extensions) {
  $Filepath1 = Resolve-Path "$File1$Extension"
  $Filepath2 = Resolve-Path "$File2$Extension"

  $hashes = 
  foreach ($Filepath in $Filepath1, $Filepath2) {
    $MD5 = [Security.Cryptography.HashAlgorithm]::Create( "MD5" )
    $stream = ([IO.StreamReader]"$Filepath").BaseStream
    -join ($MD5.ComputeHash($stream) | 
      ForEach-Object { "{0:x2}" -f $_ })
    $stream.Close()
  }

  if ($hashes[0] -eq $hashes[1])
  { "$Extension passed" }
  else
  { "$Extension failed" }
  
}