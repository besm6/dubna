{-----------------------------------------------}
{                                               }
{       c  m  d  s  2     u  n  i  t            }
{                                               }
{ this unit contains the procedure phelp.  the  }
{ help command needs no access to data in unit  }
{ 'advdata'.  it does use procedure 'show',     }
{ however, and hence USES unit cmds3.           }
{-----------------------------------------------}

{$s+}

UNIT cmds2;

  INTERFACE
  
    USES applestuff,
         {$u#5:mt1.code} advdata,
         {$u#5:cmds3.code} cmds3;
         
PROCEDURE phelp;

  IMPLEMENTATION
  
VAR

  asked:        INTEGER;
  
PROCEDURE phelp;
BEGIN

  IF asked > 2
  THEN
    show (helpspiel)
  ELSE
  BEGIN
  
    writeln ('Help is on the way');
    asked := asked + 1;
  
  END;
  
END { PROCEDURE phelp };

BEGIN

  asked := 0;

END.
