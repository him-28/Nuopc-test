* NEMSIO GrADS Output Script
* Author: Daniel Roen
* Email: daniel.rosen@noaa.gov
* Date: 2018-04-13

* PREREQUISITES:
* A. Utilities (set PATH or copy to current working directory)
*   + mkgfsnemsioctl
*   + grads
* B. GrADS Scripts (set GASCRP or copy to current working directory)
*   + cbarn.gs
*   + nemsio_shade.gs
*   + palette_rainbow.gs
* INSTRUCTIONS:
* 1. Generate grads ctl file using nemsio_util: mkgfsnemsioctl
*   > mkgfsnemsioctl <FILE>
*   (i.e. mkgfsnemsioctl flxf00)
* 2. Generate plots using GrADS script: nemsio_shade.gs
*   > grads -b -l -c 'run nemsio_shade.gs <FILE>'
*   (i.e. grads -b -l -c 'run nemsio_shade.gs flxf00.ctl')
* MODIFICATIONS:
* A. Modify variable array
*   > <EDITOR> nemsio_shade.gs
*   + Total number of variables
*     varcnt = <VARIABLE_COUNT>
*   + Variable name, title, and scale
*     variable.<VARIABLE_NUMBER> = <VARIABLE_NAME>
*     title.<VARIABLE_NUMBER>    = <VARIABLE_TITLE>
*     levels.<VARIABLE_NUMBER>   = '<MIN> <MAX> <STEP>'
* NOTES:
* A. Total number of variables
*   + Missing variable, title, or levels will cause an error
* B. Scale levels
*   + palette_rainbow.gs supports 12,15, and 25 scale levels

function main(args)
say '******************************'
say '* NEMSIO GrADS Output Script *'
say '******************************'

* Output Settings
flabel = 'nems_gfs'
ptitle = 'NEMSIO GFS Output'
* Defaults
file = 'default.ctl'
* Variable Settings
varcnt = 3
variable.1 = 'tmpsfc'
title.1    = 'Surface Temperature'
levels.1   = '210 320 10'
variable.2 = 'lhtfl_avesfc'
title.2    = 'Average Latent Heat at Surface'
levels.2   = '-150 400 50'
variable.3 = 'shtfl_avesfc'
title.3    = 'Average Sensible Heat at Surface'
levels.3   = '-150 550 50'

* Script Arguments
if  (args!='')
 file  = subwrd(args,1)
else
 say 'ERROR: Usage run nemsio_shade <file>'
 exit 1
endif

* Reinitialize
'reinit'

* Open File
'open 'file
if (rc='0')
 say "File Opened - "file
else
 say "ERROR: File Open Failed - "file
 exit 1
endif

* Set Map Grid
'set mproj latlon'
'set map 1 1 2'
'set grads off'
'set vpage 0 11 0 8.5'
*'set lon   -180 180 '
*'set lat    -90 90 '

* Set Time
'set t 1 '
'set z 1'

* Plot Settings
'set parea 1.0  10.0 1.0 8.0 '
'set dfile 1'
'set gxout shaded'
'set grads off'
'set grid  off'
'set xlopts 1 6 0.18'
'set ylopts 1 6 0.18'
'set xlint  30'
'set ylint  30'
'set string 1 l 6'
'set strsiz 0.12 0.15'
'set strsiz 0.15 0.20'

* Loop Over All Variables
v=1
while (v <= varcnt)

* Clear Plot
 'clear' 

* Calculate Levels
 lvlmin = subwrd(levels.v,1)
 lvlmax = subwrd(levels.v,2)
 lvlstp = subwrd(levels.v,3)
 lvlcnt = (( lvlmax - lvlmin ) / lvlstp ) + 1
 l = 1
 next = lvlmin
 lvls = '' 
 while (l <= lvlcnt) 
  lvls = lvls' 'next
  next = next + lvlstp
  l = l + 1 
 endwhile

* Set Palette
 'palette_rainbow 'lvlcnt
 palette = result

* Set Time
*'q time'
*time = substr(result,8,12)
 time = substr(file,5,2)

* Draw Plot
 say 'Plotting: 'variable.v' at 'time'z'
 'set clevs 'lvls
 'set ccols 'palette
 'd 'variable.v
 'cbarn'
 'draw title 'ptitle' 'title.v' at 'time'z'

* Save Plot
 'printim 'flabel'_'variable.v'_'time'z.gif gif x800 y600 white'

 v = v + 1

endwhile

exit 0
