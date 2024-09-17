program caracteres
    character(len=40) texto
    character(len=1) :: ene ! ñ
    character(len=1) :: preg ! ¿
    character(len=1) :: adm ! ¡
    character(len=1) :: aa ! á
    character(len=1) :: ee ! é
    character(len=1) :: ii ! í
    character(len=1) :: oo ! ó
    character(len=1) :: uu ! ú
    ene=char(164)
    preg=char(168)
    adm=char(173)
    aa=char(160)
    ee=char(130)
    ii=char(161)
    oo=char(162)
    uu=char(163)

    texto= adm// "ni" //ene//oo// "s!"
    print "(A)", texto
end program caracteres