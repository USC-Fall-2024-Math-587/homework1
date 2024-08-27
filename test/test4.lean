import Hw

def secret := "The only thing we have to fear is fear itself -- nameless, unreasoning, unjustified terror which paralyzes needed efforts to convert retreat into advance."

/--
info: "THEON LYTHI NGWEH AVETO FEARI SFEAR ITSEL FNAME LESSU NREAS ONING UNJUS TIFIE DTERR ORWHI CHPAR ALYZE SNEED EDEFF ORTST OCONV ERTRE TREAT INTOA DVANC E"
-/
#guard_msgs in
#eval encode (encode secret 4) 22
