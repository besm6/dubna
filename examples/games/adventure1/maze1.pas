  (*(*
  *)
  (**************************************)
  (*       p   m   a   z   e            *)
  (**************************************)
  
  PROCEDURE pmaze;
  TYPE
  
    mazerooms = (m1,m2,m3,m4,m5,m6,m7,m8,
                 m9,m10,m11,m12,m13,m14,
                 m15,m16,m17,m18,m19);
  VAR
  
    mazeloc:      mazerooms;
    bitset:       ARRAY[directions] OF BOOLEAN;
    
    FUNCTION bittest
             (v: INTEGER; dir: directions): BOOLEAN;
    BEGIN
    
      IF ((v DIV twopow[dir]) MOD 2)=1
      THEN
        bittest := true
      ELSE
        bittest := false
      (* endif *);
      
    END (* FUNCTION bittest *);
    
    PROCEDURE describe(wh: INTEGER);
    VAR
      dir: directions;
      
    BEGIN
    
      writeln ('You are in a maze of featureless');
      writeln ('passages.  There are exits visible');
      writeln ('in the following directions:');
      
      IF bittest (wh,n) THEN write ('n ');
      IF bittest (wh,s) THEN write ('s ');
      IF bittest (wh,e) THEN write ('e ');
      IF bittest (wh,w) THEN write ('w ');
      IF bittest (wh,u) THEN write ('u ');
      IF bittest (wh,d) THEN write ('d ');
      
      writeln;
      
    end (* procedure describe *);
    
  PROCEDURE sameplace;
  BEGIN
  
    writeln ('You have crawled around some');
    writeln ('twisted tunnels and wound up');
    writeln ('where you began.');
    
  END (* PROCEDURE sameplace *);
  
  PROCEDURE treasure;
  BEGIN
  
    IF NOT carrying
    THEN
    BEGIN
    
      IF readmsg
      THEN
      BEGIN
      
        writeln ('The treasure is here!!');
        writeln ('Do you want to take it now?');
        
        readln (command);
        
        IF (command = 'yes') OR
           (command = 'y')
        THEN
          carrying := true
        (* endif *);
        
      END
      ELSE
      BEGIN
      
        writeln ('The light is extremely dim here');
        writeln ('You better get out or risk');
        writeln ('falling into a pit.');
        
      END (* IF readmsg *);
      
    END (* IF NOT carrying *);
  
  END (* PROCEDURE treasure *);
  
  PROCEDURE pm1;
  BEGIN
  
    writeln ('You are in a maze of featureless');
    writeln ('passages.');
    writeln ('From here you can go south, east,');
    writeln ('west, or up.');
    
    CASE whichway OF
    
      s:        location := ladder;
      e:        mazeloc  := m2;
      w:        mazeloc  := m4;
      u:        location := steam;
      n,d:      noway;
      
    END;
    
  END (* pm1 *);
  
  PROCEDURE pm2;
  BEGIN
   
    describe(nw);
    
    CASE whichway OF
    
      n:        mazeloc := m1;
      w:        sameplace;
      e,s,u,d:  noway;
      
    END;
    
  END (* PROCEDURE pm2 *);
  
  PROCEDURE pm3;
  BEGIN
  
    describe(ne);
    
    CASE whichway OF
    
      n:        mazeloc := m1;
      e:        sameplace;
      s,w,u,d:  noway;
      
    END;
    
  END (* PROCEDURE pm3 *);
  
  PROCEDURE pm4;
  BEGIN
  
    describe(sew);
  
    CASE whichway OF
  
      s:        mazeloc := m7;
      e:        mazeloc := m3;
      w:        mazeloc := m5;
      n,u,d:    noway;
    
    END;
  
  END (* PROCEDURE pm4 *);

  PROCEDURE pm5;
  BEGIN

    describe(nonly);
  
    CASE whichway OF
    
      n:        mazeloc :=m1;
  
      e,w,s,u,d:        noway;
    
    END;
  
  END (* PROCEDURE pm5 *);

  PROCEDURE pm6;
  BEGIN

    describe(ne);
  
    CASE whichway OF
  
      n:        mazeloc := m4;
      e:        sameplace;
    
      s,w,u,d:  noway;
    
    END;
  
  END (* PROCEDURE pm6 *);

  PROCEDURE pm7;
  BEGIN

    describe(nsew);

    CASE whichway OF

      n:        mazeloc := m5;
      s:        mazeloc := m9;
      e:        mazeloc := m6;
      w:        mazeloc := m8;
  
      u,d:      noway;
  
    END;

  END (* PROCEDURE pm7 *);
  
  PROCEDURE pm8;
  BEGIN

    describe(nw);
  
    CASE whichway OF
  
      n:        mazeloc := m5;
      w:        sameplace;
    
      e,s,u,d:  noway;
    
    END;
  
  END (* PROCEDURE pm8 *);

  PROCEDURE pm9;
  BEGIN

    describe(sw);
  
    CASE whichway OF
  
      s:        mazeloc := m11;
      w:        mazeloc := m10;
    
      n,e,u,d:  noway;
    
    END;
  
  END (* PROCEDURE pm9 *);
  
  PROCEDURE pm10;
  BEGIN

    describe(ns);
  
    CASE whichway OF
  
      n:        mazeloc := m8;
      s:        sameplace;
    
      e,w,u,d:  noway;
    
    END;
  
  END (* PROCEDURE pm10 *);

  PROCEDURE pm11;
  BEGIN

    describe(newud);
  
    CASE whichway OF

      n:        mazeloc := m9;
      e:        mazeloc := m6;
      w:        mazeloc := m10;
      u:        mazeloc := m1;
      d:        mazeloc := m12;
    
      s:        noway;

    END;
  
  END (* PROCEDURE pm11 *);

  PROCEDURE pm12;
  BEGIN

    describe(dn);
  
    CASE whichway OF

      n:        mazeloc := m13;
      d:        mazeloc := m16;
    
      e,s,w,u:  noway;
    
    END;
  
  END (* PROCEDURE pm12 *);

  PROCEDURE pm13;
  BEGIN

    describe(dn);
  
    CASE whichway OF

      n:        mazeloc := m14;
      d:        mazeloc := m17;
    
      e,s,w,u:  noway;

    END;
  
  END (* PROCEDURE pm13 *);

  PROCEDURE pm14;
  BEGIN

    describe(dn);
  
    CASE whichway OF

      n:        mazeloc := m15;
      d:        mazeloc := m18;
    
      e,s,w,u:  noway;

    END;
  
  END (* PROCEDURE pm14 *);

  PROCEDURE pm15;
  BEGIN

    describe(ud);
  
    CASE whichway OF

      u:        mazeloc := m1;
      d:        mazeloc := m19;
    
      n,s,e,w:  noway;
    
    END;
  
  END (* PROCEDURE pm15 *);

  PROCEDURE pm16;
  BEGIN

    describe(ns);
  
    CASE whichway OF
    
      n:        mazeloc := m17;
      s:        sameplace;
    
      e,w,u,d:  noway;

    END;
  
  END (* PROCEDURE pm16 *);

  PROCEDURE pm17;
  BEGIN

    describe(ns);
  
    CASE whichway OF

      n:        mazeloc := m18;
      s:        mazeloc := m16;
      e,w,u,d:  noway;

    END;
  
  END (* PROCEDURE pm17 *);

  PROCEDURE pm18;
  BEGIN

    describe(ns);
  
    CASE whichway OF

      n:        mazeloc := m19;
      s:        mazeloc := m17;

      e,w,u,d:  noway;

    END;
  
  END (* PROCEDURE pm18 *);

  PROCEDURE pm19;
  BEGIN

    describe(su);
    treasure;
  
    CASE whichway OF

      s:        mazeloc := m18;
      u:        mazeloc := m15;
    
      n,e,w,d:  noway;

    END;
  
  END (* PROCEDURE pm19 *);
  
BEGIN  (* PROCEDURE pmaze *);

  mazeloc := m1;
  
  REPEAT
  
    CASE mazeloc OF
    
      m1:       pm1;
      m2:       pm2;
      m3:       pm3;
      m4:       pm4;
      m5:       pm5;
      m6:       pm6;
      m7:       pm7;
      m8:       pm8;
      m9:       pm9;
      m10:      pm10;
      m11:      pm11;
      m12:      pm12;
      m13:      pm13;
      m14:      pm14;
      m15:      pm15;
      m16:      pm16;
      m17:      pm17;
      m18:      pm18;
      m19:      pm19;
      
    END (* CASE mazeloc *);
    
  UNTIL location <> maze;
  
END (* PROCEDURE pmaze *);
      
