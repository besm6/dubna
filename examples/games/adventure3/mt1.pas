{---------------------------}
{ source file: mt1.text     }
{---------------------------}

{----------------------------------------------}
{                                              }
{  r  o  o  m  s         u  n  i  t            }
{                                              }
{  types and variables used by all other units }
{  in mountain  adventure.  this includes the  }
{  descs enumerated type, extended to account  }
{  for some descriptions that are not actual   }
{  game locations.  the goodies type accounts  }
{  for all objects in the adventure and the    }
{  objname array contains names for all of     }
{  them (for use in descriptions and in        }
{   commands).                                 }
{                                              }
{----------------------------------------------}

{$s+}

UNIT advdata;

  INTERFACE
  
    USES applestuff;
    
TYPE

  descs = (start, trail1, trail2, trail3, trail4,
          trail5, trail6, col1, col2, col3, col4,
          rubble1, rubble2, mound, monastery,
          cellar, cave, slope1, slope2, slope3,
          slope4, slope5, rut1, rut2, rut3,
          steep1, steep2, steep3, steep4, steep5,
          peak1, peak2, peak3, peak4, peak5,
          saddle1, saddle2, saddle3, mtcave1a,
          mtcave1b, mtcave1c, mtcave2a, mtcave2b,
          mtcave2c, gully1, gully2, hill1,
          trolls, ridge1, ridge2, wmm, chasm,
          nowhere, cellardown, fountain, trtalk,
          wmmblab, wmmharp, wmmhello, wmmsp1,
          wmmsp2, wmmsp3, wmmsp4, wmmsp5,
          tgablure, tgabbones, trhungry,
          trfollow, helpspiel);
          
  rooms = start..nowhere;
  
  goodies = (nuggets, silver, diamonds, beryl,
             amethyst, carvings, wine, wineskins,
             flowers, axe, hammer, flint, keys,
             antlers, talons, feathers, eggs,
             walkstick, leather, cymbals, drum,
             bones, scroll, wheel, ramshorn,
             berries, knife, burner, incense,
             tooth, noobj);
             
  fakes      = nuggets..wineskins;
  trash      = flowers..bones;
  wmmgoody   = scroll..tooth;
  collection = SET OF goodies;
  
VAR

  location:     rooms;
  trollocs:     SET OF rooms;
  
  wmmwants:     collection;
  wmmhas:       collection;
  wmmfakes:     collection;
  stash:        collection;
  
  whatshere:    ARRAY[rooms] OF collection;
  visited:      ARRAY[rooms] OF INTEGER;
  objname:      ARRAY[goodies] OF STRING[15];
  
  turns:        INTEGER;
  troltime:     INTEGER;
  wmmcount:     INTEGER;
  trlives:      BOOLEAN;
                {true when trolls still around}
  saidwadda:    BOOLEAN;
  isopen:       BOOLEAN;
  eaten:        BOOLEAN;
  killed:       BOOLEAN;
  done:         BOOLEAN;
  
FUNCTION rand (low, high: INTEGER) : INTEGER;

  IMPLEMENTATION
  
VAR

  r:    rooms;
  { loop control for initialization }
  
FUNCTION rand;
VAR
  mx, c, d: INTEGER;
BEGIN
  rand :=  0;
  IF low = high
  THEN
    rand := low
    
  ELSE
  BEGIN
  
    c := high - low + 1;
    mx := (maxint - high + low) DIV c + 1;
    mx := mx * (high - low) + (mx - 1);
    
    REPEAT
      d := random
    UNTIL d <= mx;
    rand := low + d MOD c;
    
  END { IF low = high };
  
END { FUNCTION rand };

PROCEDURE init1;
BEGIN

  location := start;
  
  objname[nuggets]      := 'nuggets';
  objname[silver]       := 'silver';
  objname[diamonds]     := 'diamonds';
  objname[beryl]        := 'beryl';
  objname[amethyst]     := 'amethyst';
  objname[carvings]     := 'carvings';
  objname[wine]         := 'wine';
  objname[wineskins]    := 'wineskins';
  objname[flowers]      := 'flowers';
  objname[axe]          := 'axe';
  objname[hammer]       := 'hammer';
  objname[flint]        := 'flint';
  objname[keys]         := 'keys';
  objname[antlers]      := 'antlers';
  objname[talons]       := 'talons';
  objname[feathers]     := 'feathers';
  objname[eggs]         := 'eagle-eggs';
  objname[walkstick]    := 'walking-stick';
  objname[leather]      := 'leather';
  objname[cymbals]      := 'cymbals';
  objname[drum]         := 'drum';
  objname[bones]        := 'bones';
  objname[scroll]       := 'scroll';
  objname[wheel]        := 'prayer-wheel';
  objname[ramshorn]     := 'ramshorn';
  objname[berries]      := 'berries';
  objname[knife]        := 'knife';
  objname[burner]       := 'incense-burner';
  objname[incense]      := 'incense';
  objname[tooth]        := 'tooth';
  
  FOR r := start TO chasm DO
  
  BEGIN
  
    whatshere[r] := [];
    visited[r]   := 0;
    
  END { FOR r := start ... };
  
END { PROCEDURE init1 };

PROCEDURE init2;
BEGIN
  
  whatshere[peak1]      := [scroll];
  whatshere[peak2]      := [ramshorn];
  whatshere[peak3]      := [incense];
  whatshere[peak4]      := [burner];
  whatshere[peak5]      := [wheel];
  whatshere[trail3]     := [walkstick, axe];
  whatshere[cave]       := [bones];
  whatshere[mound]      := [flowers];
  whatshere[mtcave1b]   := [knife];
  whatshere[rut3]       := [nuggets];
  whatshere[rubble2]    := [silver];
  whatshere[trolls]     := [amethyst,
                            wine,
                            wineskin];
  whatshere[gully1]     := [beryl];
  whatshere[ridge2]     := [carvings,
                            keys,
                            berries];
  whatshere[mtcave2a]   := [flint];
  whatshere[trail6]     := [antlers];
  whatshere[steep5]     := [talons];
  whatshere[saddle2]    := [leather];
  whatshere[steep4]     := [eggs];
  whatshere[monastery]  := [drum, cymbals];
  whatshere[cellar]     := [hammer];
  whatshere[steep3]     := [feathers];
  whatshere[mtcave1c]   := [diamonds];
  
  trollocs := [trolls, slope2, steep2, peak2,
               saddle1, col1, saddle2,
               mtcave1a, mtcave1b, trail5,
               trail2, rut1, ridge2];
  wmmwants  := [scroll..tooth];
  wmmhas    := [];
  wmmfakes  := [nuggets..wineskins];
  stash     := [];
  turns     := 0;
  troltime  := 0;
  wmmcount  := 0;
  trlives   := true;
  saidwadda := false;
  isopen    := false;
  eaten     := false;
  done      := false;
  
END { PROCEDURE init2 };

BEGIN

  init1;
  init2;
  
END.
