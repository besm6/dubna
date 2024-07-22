{-----------------------------------------------}
{                                               }
{     p  r  o  b  s        u  n  i  t           }
{                                               }
{ this unit contains the single function:       }
{ solved.  solved is passed an argument of type }
{ 'problems'.  the argument corresponds to one  }
{ of the boolean expressions used to detect the }
{ solution of problems and compenents of        }
{ problems.  the rest of the adventure code     }
{ uses code like:                               }
{                                               }
{       IF NOT solved (luretrolls)              }
{       THEN                                    }
{                                               }
{ instead of using the complex expressions.     }
{ the details of the problem expressions are    }
{ contained here and may be changed without     }
{ forcing the rest of the adventure game code   }
{ change as well.                               }
{                                               }
{-----------------------------------------------}

{$s+}

UNIT probs;

  INTERFACE
  
    USES  applestuff,
          {$u#5:mt1.code} advdata;
          
TYPE

  problems = (wmmhappy, luretrolls,
              bonetrolls, canopen, ftofyouth);
              
FUNCTION solved (which: problems) : BOOLEAN;

  IMPLEMENTATION
  
FUNCTION solved;
BEGIN

  solved := false;
  
  CASE which OF
  
    wmmhappy:   solved :=
                  (wmmhas = wmmwants) AND
                  (whatshere[wmm] * wmmfakes = []);
                  
                  
    luretrolls: solved :=
                  (troltime > 0 ) AND
                  ((turns - troltime) > 0) AND
                  ((turns - troltime) < 25);
                  
    bonetrolls: solved :=
                  bones IN whatshere[mtcave1a];
                  
    canopen:    solved :=
                  (location = cellar) AND
                  (keys IN stash);
                  
    ftofyouth:  solved :=
                  (wmmhas = wmmwants) AND
                  (whatshere[wmm] * wmmfakes = []) AND
                  (location = cellar);
                  
  END { CASE which OF };
  
END { FUNCTION solved };

BEGIN
END.
