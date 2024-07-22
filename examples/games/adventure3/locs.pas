{-----------------------------------------------}
{                                               }
{       l  o  c  s        u  n  i  t            }
{                                               }
{ the locs unit contains procedures needed for  }
{ handling special situations at certain game   }
{ locations.  in mtadvent the only such place   }
{ is 'wmm'.  therefore, this implementation of  }
{ locs contains only the procedure pwmm.        }
{                                               }
{-----------------------------------------------}

{$s+}

UNIT locs;

  INTERFACE
    
    USES  applestuff,
          {$u#5:mt1.code} advdata,
          {$u#5:probs.code} probs,
          {$u#5:cmds3.code} cmds3,
          {$u#5:cmds2.code} cmds2,
          {$u#5:cmds1.code} cmds1;
    
PROCEDURE pwmm;

  IMPLEMENTATION
  
VAR

  atwmm:        INTEGER;
  
{--------------------------------------}
{        p   w   m   m                 }
{                                      }
{ handle special occurrences at the    }
{ location wmm.                        }
{--------------------------------------}

PROCEDURE pwmm;
VAR
  r:    INTEGER;
  
BEGIN

  atwmm := atwmm + 1;

  IF solved (wmmhappy)
  THEN
    show (wmmblab)
  { END IF };
  
  IF wmmcount >=3
  THEN
    show (wmmharp)
  { END IF };
  
  IF atwmm > 1
  THEN
  BEGIN
  
    r := rand (1,5);
    
    CASE r OF
    
     1: show (wmmsp1);
     2: show (wmmsp2);
     3: show (wmmsp3);
     4: show (wmmsp4);
     5: show (wmmsp5);
    
    END { CASE r OF };
    
  END { IF atwmm > 1 };
  
  IF atwmm = 1
  THEN
    show (wmmhello)
  { END IF };
  
END { PROCEDURE pwmm };

BEGIN
  
  atwmm := 0;
  
END.
