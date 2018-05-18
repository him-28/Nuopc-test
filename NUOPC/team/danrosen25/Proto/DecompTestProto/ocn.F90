module OCN

  !-----------------------------------------------------------------------------
  ! OCN Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS      => SetServices, &
    model_label_SetClock  => label_SetClock, &
    model_label_Advance   => label_Advance
  
  implicit none
  
  private
  
  public SetServices

  INTEGER, DIMENSION(2,2,256) :: blockList_sync = &
    (/ &
      (/(/  1,  1/),(/ 40, 40/)/), &
      (/(/ 41,  1/),(/ 79, 40/)/), &
      (/(/ 80,  1/),(/118, 40/)/), &
      (/(/119,  1/),(/157, 40/)/), &
      (/(/158,  1/),(/196, 40/)/), &
      (/(/197,  1/),(/235, 40/)/), &
      (/(/236,  1/),(/274, 40/)/), &
      (/(/275,  1/),(/313, 40/)/), &
      (/(/314,  1/),(/352, 40/)/), &
      (/(/353,  1/),(/391, 40/)/), &
      (/(/392,  1/),(/430, 40/)/), &
      (/(/431,  1/),(/469, 40/)/), &
      (/(/470,  1/),(/508, 40/)/), &
      (/(/509,  1/),(/547, 40/)/), &
      (/(/548,  1/),(/587, 40/)/), &
      (/(/588,  1/),(/628, 40/)/), &
      (/(/  1, 41/),(/ 40, 79/)/), &
      (/(/ 41, 41/),(/ 79, 79/)/), &
      (/(/ 80, 41/),(/118, 79/)/), &
      (/(/119, 41/),(/157, 79/)/), &
      (/(/158, 41/),(/196, 79/)/), &
      (/(/197, 41/),(/235, 79/)/), &
      (/(/236, 41/),(/274, 79/)/), &
      (/(/275, 41/),(/313, 79/)/), &
      (/(/314, 41/),(/352, 79/)/), &
      (/(/353, 41/),(/391, 79/)/), &
      (/(/392, 41/),(/430, 79/)/), &
      (/(/431, 41/),(/469, 79/)/), &
      (/(/470, 41/),(/508, 79/)/), &
      (/(/509, 41/),(/547, 79/)/), &
      (/(/548, 41/),(/587, 79/)/), &
      (/(/588, 41/),(/628, 79/)/), &
      (/(/  1, 80/),(/ 40,118/)/), &
      (/(/ 41, 80/),(/ 79,118/)/), &
      (/(/ 80, 80/),(/118,118/)/), &
      (/(/119, 80/),(/157,118/)/), &
      (/(/158, 80/),(/196,118/)/), &
      (/(/197, 80/),(/235,118/)/), &
      (/(/236, 80/),(/274,118/)/), &
      (/(/275, 80/),(/313,118/)/), &
      (/(/314, 80/),(/352,118/)/), &
      (/(/353, 80/),(/391,118/)/), &
      (/(/392, 80/),(/430,118/)/), &
      (/(/431, 80/),(/469,118/)/), &
      (/(/470, 80/),(/508,118/)/), &
      (/(/509, 80/),(/547,118/)/), &
      (/(/548, 80/),(/587,118/)/), &
      (/(/588, 80/),(/628,118/)/), &
      (/(/  1,119/),(/ 40,157/)/), &
      (/(/ 41,119/),(/ 79,157/)/), &
      (/(/ 80,119/),(/118,157/)/), &
      (/(/119,119/),(/157,157/)/), &
      (/(/158,119/),(/196,157/)/), &
      (/(/197,119/),(/235,157/)/), &
      (/(/236,119/),(/274,157/)/), &
      (/(/275,119/),(/313,157/)/), &
      (/(/314,119/),(/352,157/)/), &
      (/(/353,119/),(/391,157/)/), &
      (/(/392,119/),(/430,157/)/), &
      (/(/431,119/),(/469,157/)/), &
      (/(/470,119/),(/508,157/)/), &
      (/(/509,119/),(/547,157/)/), &
      (/(/548,119/),(/587,157/)/), &
      (/(/588,119/),(/628,157/)/), &
      (/(/  1,158/),(/ 40,196/)/), &
      (/(/ 41,158/),(/ 79,196/)/), &
      (/(/ 80,158/),(/118,196/)/), &
      (/(/119,158/),(/157,196/)/), &
      (/(/158,158/),(/196,196/)/), &
      (/(/197,158/),(/235,196/)/), &
      (/(/236,158/),(/274,196/)/), &
      (/(/275,158/),(/313,196/)/), &
      (/(/314,158/),(/352,196/)/), &
      (/(/353,158/),(/391,196/)/), &
      (/(/392,158/),(/430,196/)/), &
      (/(/431,158/),(/469,196/)/), &
      (/(/470,158/),(/508,196/)/), &
      (/(/509,158/),(/547,196/)/), &
      (/(/548,158/),(/587,196/)/), &
      (/(/588,158/),(/628,196/)/), &
      (/(/  1,197/),(/ 40,235/)/), &
      (/(/ 41,197/),(/ 79,235/)/), &
      (/(/ 80,197/),(/118,235/)/), &
      (/(/119,197/),(/157,235/)/), &
      (/(/158,197/),(/196,235/)/), &
      (/(/197,197/),(/235,235/)/), &
      (/(/236,197/),(/274,235/)/), &
      (/(/275,197/),(/313,235/)/), &
      (/(/314,197/),(/352,235/)/), &
      (/(/353,197/),(/391,235/)/), &
      (/(/392,197/),(/430,235/)/), &
      (/(/431,197/),(/469,235/)/), &
      (/(/470,197/),(/508,235/)/), &
      (/(/509,197/),(/547,235/)/), &
      (/(/548,197/),(/587,235/)/), &
      (/(/588,197/),(/628,235/)/), &
      (/(/  1,236/),(/ 40,274/)/), &
      (/(/ 41,236/),(/ 79,274/)/), &
      (/(/ 80,236/),(/118,274/)/), &
      (/(/119,236/),(/157,274/)/), &
      (/(/158,236/),(/196,274/)/), &
      (/(/197,236/),(/235,274/)/), &
      (/(/236,236/),(/274,274/)/), &
      (/(/275,236/),(/313,274/)/), &
      (/(/314,236/),(/352,274/)/), &
      (/(/353,236/),(/391,274/)/), &
      (/(/392,236/),(/430,274/)/), &
      (/(/431,236/),(/469,274/)/), &
      (/(/470,236/),(/508,274/)/), &
      (/(/509,236/),(/547,274/)/), &
      (/(/548,236/),(/587,274/)/), &
      (/(/588,236/),(/628,274/)/), &
      (/(/  1,275/),(/ 40,313/)/), &
      (/(/ 41,275/),(/ 79,313/)/), &
      (/(/ 80,275/),(/118,313/)/), &
      (/(/119,275/),(/157,313/)/), &
      (/(/158,275/),(/196,313/)/), &
      (/(/197,275/),(/235,313/)/), &
      (/(/236,275/),(/274,313/)/), &
      (/(/275,275/),(/313,313/)/), &
      (/(/314,275/),(/352,313/)/), &
      (/(/353,275/),(/391,313/)/), &
      (/(/392,275/),(/430,313/)/), &
      (/(/431,275/),(/469,313/)/), &
      (/(/470,275/),(/508,313/)/), &
      (/(/509,275/),(/547,313/)/), &
      (/(/548,275/),(/587,313/)/), &
      (/(/588,275/),(/628,313/)/), &
      (/(/  1,314/),(/ 40,352/)/), &
      (/(/ 41,314/),(/ 79,352/)/), &
      (/(/ 80,314/),(/118,352/)/), &
      (/(/119,314/),(/157,352/)/), &
      (/(/158,314/),(/196,352/)/), &
      (/(/197,314/),(/235,352/)/), &
      (/(/236,314/),(/274,352/)/), &
      (/(/275,314/),(/313,352/)/), &
      (/(/314,314/),(/352,352/)/), &
      (/(/353,314/),(/391,352/)/), &
      (/(/392,314/),(/430,352/)/), &
      (/(/431,314/),(/469,352/)/), &
      (/(/470,314/),(/508,352/)/), &
      (/(/509,314/),(/547,352/)/), &
      (/(/548,314/),(/587,352/)/), &
      (/(/588,314/),(/628,352/)/), &
      (/(/  1,353/),(/ 40,391/)/), &
      (/(/ 41,353/),(/ 79,391/)/), &
      (/(/ 80,353/),(/118,391/)/), &
      (/(/119,353/),(/157,391/)/), &
      (/(/158,353/),(/196,391/)/), &
      (/(/197,353/),(/235,391/)/), &
      (/(/236,353/),(/274,391/)/), &
      (/(/275,353/),(/313,391/)/), &
      (/(/314,353/),(/352,391/)/), &
      (/(/353,353/),(/391,391/)/), &
      (/(/392,353/),(/430,391/)/), &
      (/(/431,353/),(/469,391/)/), &
      (/(/470,353/),(/508,391/)/), &
      (/(/509,353/),(/547,391/)/), &
      (/(/548,353/),(/587,391/)/), &
      (/(/588,353/),(/628,391/)/), &
      (/(/  1,392/),(/ 40,430/)/), &
      (/(/ 41,392/),(/ 79,430/)/), &
      (/(/ 80,392/),(/118,430/)/), &
      (/(/119,392/),(/157,430/)/), &
      (/(/158,392/),(/196,430/)/), &
      (/(/197,392/),(/235,430/)/), &
      (/(/236,392/),(/274,430/)/), &
      (/(/275,392/),(/313,430/)/), &
      (/(/314,392/),(/352,430/)/), &
      (/(/353,392/),(/391,430/)/), &
      (/(/392,392/),(/430,430/)/), &
      (/(/431,392/),(/469,430/)/), &
      (/(/470,392/),(/508,430/)/), &
      (/(/509,392/),(/547,430/)/), &
      (/(/548,392/),(/587,430/)/), &
      (/(/588,392/),(/628,430/)/), &
      (/(/  1,431/),(/ 40,469/)/), &
      (/(/ 41,431/),(/ 79,469/)/), &
      (/(/ 80,431/),(/118,469/)/), &
      (/(/119,431/),(/157,469/)/), &
      (/(/158,431/),(/196,469/)/), &
      (/(/197,431/),(/235,469/)/), &
      (/(/236,431/),(/274,469/)/), &
      (/(/275,431/),(/313,469/)/), &
      (/(/314,431/),(/352,469/)/), &
      (/(/353,431/),(/391,469/)/), &
      (/(/392,431/),(/430,469/)/), &
      (/(/431,431/),(/469,469/)/), &
      (/(/470,431/),(/508,469/)/), &
      (/(/509,431/),(/547,469/)/), &
      (/(/548,431/),(/587,469/)/), &
      (/(/588,431/),(/628,469/)/), &
      (/(/  1,470/),(/ 40,508/)/), &
      (/(/ 41,470/),(/ 79,508/)/), &
      (/(/ 80,470/),(/118,508/)/), &
      (/(/119,470/),(/157,508/)/), &
      (/(/158,470/),(/196,508/)/), &
      (/(/197,470/),(/235,508/)/), &
      (/(/236,470/),(/274,508/)/), &
      (/(/275,470/),(/313,508/)/), &
      (/(/314,470/),(/352,508/)/), &
      (/(/353,470/),(/391,508/)/), &
      (/(/392,470/),(/430,508/)/), &
      (/(/431,470/),(/469,508/)/), &
      (/(/470,470/),(/508,508/)/), &
      (/(/509,470/),(/547,508/)/), &
      (/(/548,470/),(/587,508/)/), &
      (/(/588,470/),(/628,508/)/), &
      (/(/  1,509/),(/ 40,547/)/), &
      (/(/ 41,509/),(/ 79,547/)/), &
      (/(/ 80,509/),(/118,547/)/), &
      (/(/119,509/),(/157,547/)/), &
      (/(/158,509/),(/196,547/)/), &
      (/(/197,509/),(/235,547/)/), &
      (/(/236,509/),(/274,547/)/), &
      (/(/275,509/),(/313,547/)/), &
      (/(/314,509/),(/352,547/)/), &
      (/(/353,509/),(/391,547/)/), &
      (/(/392,509/),(/430,547/)/), &
      (/(/431,509/),(/469,547/)/), &
      (/(/470,509/),(/508,547/)/), &
      (/(/509,509/),(/547,547/)/), &
      (/(/548,509/),(/587,547/)/), &
      (/(/588,509/),(/628,547/)/), &
      (/(/  1,548/),(/ 40,587/)/), &
      (/(/ 41,548/),(/ 79,587/)/), &
      (/(/ 80,548/),(/118,587/)/), &
      (/(/119,548/),(/157,587/)/), &
      (/(/158,548/),(/196,587/)/), &
      (/(/197,548/),(/235,587/)/), &
      (/(/236,548/),(/274,587/)/), &
      (/(/275,548/),(/313,587/)/), &
      (/(/314,548/),(/352,587/)/), &
      (/(/353,548/),(/391,587/)/), &
      (/(/392,548/),(/430,587/)/), &
      (/(/431,548/),(/469,587/)/), &
      (/(/470,548/),(/508,587/)/), &
      (/(/509,548/),(/547,587/)/), &
      (/(/548,548/),(/587,587/)/), &
      (/(/588,548/),(/628,587/)/), &
      (/(/  1,588/),(/ 40,628/)/), &
      (/(/ 41,588/),(/ 79,628/)/), &
      (/(/ 80,588/),(/118,628/)/), &
      (/(/119,588/),(/157,628/)/), &
      (/(/158,588/),(/196,628/)/), &
      (/(/197,588/),(/235,628/)/), &
      (/(/236,588/),(/274,628/)/), &
      (/(/275,588/),(/313,628/)/), &
      (/(/314,588/),(/352,628/)/), &
      (/(/353,588/),(/391,628/)/), &
      (/(/392,588/),(/430,628/)/), &
      (/(/431,588/),(/469,628/)/), &
      (/(/470,588/),(/508,628/)/), &
      (/(/509,588/),(/547,628/)/), &
      (/(/548,588/),(/587,628/)/), &
      (/(/588,588/),(/628,628/)/)  &
    /)

  INTEGER, DIMENSION(2,2,256) :: blockList_noSync = &
    (/ &
      (/(/  1,  1/),(/ 40, 40/)/), &
      (/(/ 41,  1/),(/ 80, 40/)/), &
      (/(/ 81,  1/),(/119, 40/)/), &
      (/(/120,  1/),(/158, 40/)/), &
      (/(/159,  1/),(/197, 40/)/), &
      (/(/198,  1/),(/236, 40/)/), &
      (/(/237,  1/),(/275, 40/)/), &
      (/(/276,  1/),(/314, 40/)/), &
      (/(/315,  1/),(/353, 40/)/), &
      (/(/354,  1/),(/392, 40/)/), &
      (/(/393,  1/),(/431, 40/)/), &
      (/(/432,  1/),(/470, 40/)/), &
      (/(/471,  1/),(/509, 40/)/), &
      (/(/510,  1/),(/549, 40/)/), &
      (/(/550,  1/),(/589, 40/)/), &
      (/(/590,  1/),(/628, 40/)/), &
      (/(/  1, 41/),(/ 40, 80/)/), &
      (/(/ 41, 41/),(/ 80, 80/)/), &
      (/(/ 81, 41/),(/119, 80/)/), &
      (/(/120, 41/),(/158, 80/)/), &
      (/(/159, 41/),(/197, 80/)/), &
      (/(/198, 41/),(/236, 80/)/), &
      (/(/237, 41/),(/275, 80/)/), &
      (/(/276, 41/),(/314, 80/)/), &
      (/(/315, 41/),(/353, 80/)/), &
      (/(/354, 41/),(/392, 80/)/), &
      (/(/393, 41/),(/431, 80/)/), &
      (/(/432, 41/),(/470, 80/)/), &
      (/(/471, 41/),(/509, 80/)/), &
      (/(/510, 41/),(/549, 80/)/), &
      (/(/550, 41/),(/589, 80/)/), &
      (/(/590, 41/),(/628, 80/)/), &
      (/(/  1, 81/),(/ 40,119/)/), &
      (/(/ 41, 81/),(/ 80,119/)/), &
      (/(/ 81, 81/),(/119,119/)/), &
      (/(/120, 81/),(/158,119/)/), &
      (/(/159, 81/),(/197,119/)/), &
      (/(/198, 81/),(/236,119/)/), &
      (/(/237, 81/),(/275,119/)/), &
      (/(/276, 81/),(/314,119/)/), &
      (/(/315, 81/),(/353,119/)/), &
      (/(/354, 81/),(/392,119/)/), &
      (/(/393, 81/),(/431,119/)/), &
      (/(/432, 81/),(/470,119/)/), &
      (/(/471, 81/),(/509,119/)/), &
      (/(/510, 81/),(/549,119/)/), &
      (/(/550, 81/),(/589,119/)/), &
      (/(/590, 81/),(/628,119/)/), &
      (/(/  1,120/),(/ 40,158/)/), &
      (/(/ 41,120/),(/ 80,158/)/), &
      (/(/ 81,120/),(/119,158/)/), &
      (/(/120,120/),(/158,158/)/), &
      (/(/159,120/),(/197,158/)/), &
      (/(/198,120/),(/236,158/)/), &
      (/(/237,120/),(/275,158/)/), &
      (/(/276,120/),(/314,158/)/), &
      (/(/315,120/),(/353,158/)/), &
      (/(/354,120/),(/392,158/)/), &
      (/(/393,120/),(/431,158/)/), &
      (/(/432,120/),(/470,158/)/), &
      (/(/471,120/),(/509,158/)/), &
      (/(/510,120/),(/549,158/)/), &
      (/(/550,120/),(/589,158/)/), &
      (/(/590,120/),(/628,158/)/), &
      (/(/  1,159/),(/ 40,197/)/), &
      (/(/ 41,159/),(/ 80,197/)/), &
      (/(/ 81,159/),(/119,197/)/), &
      (/(/120,159/),(/158,197/)/), &
      (/(/159,159/),(/197,197/)/), &
      (/(/198,159/),(/236,197/)/), &
      (/(/237,159/),(/275,197/)/), &
      (/(/276,159/),(/314,197/)/), &
      (/(/315,159/),(/353,197/)/), &
      (/(/354,159/),(/392,197/)/), &
      (/(/393,159/),(/431,197/)/), &
      (/(/432,159/),(/470,197/)/), &
      (/(/471,159/),(/509,197/)/), &
      (/(/510,159/),(/549,197/)/), &
      (/(/550,159/),(/589,197/)/), &
      (/(/590,159/),(/628,197/)/), &
      (/(/  1,198/),(/ 40,236/)/), &
      (/(/ 41,198/),(/ 80,236/)/), &
      (/(/ 81,198/),(/119,236/)/), &
      (/(/120,198/),(/158,236/)/), &
      (/(/159,198/),(/197,236/)/), &
      (/(/198,198/),(/236,236/)/), &
      (/(/237,198/),(/275,236/)/), &
      (/(/276,198/),(/314,236/)/), &
      (/(/315,198/),(/353,236/)/), &
      (/(/354,198/),(/392,236/)/), &
      (/(/393,198/),(/431,236/)/), &
      (/(/432,198/),(/470,236/)/), &
      (/(/471,198/),(/509,236/)/), &
      (/(/510,198/),(/549,236/)/), &
      (/(/550,198/),(/589,236/)/), &
      (/(/590,198/),(/628,236/)/), &
      (/(/  1,237/),(/ 40,275/)/), &
      (/(/ 41,237/),(/ 80,275/)/), &
      (/(/ 81,237/),(/119,275/)/), &
      (/(/120,237/),(/158,275/)/), &
      (/(/159,237/),(/197,275/)/), &
      (/(/198,237/),(/236,275/)/), &
      (/(/237,237/),(/275,275/)/), &
      (/(/276,237/),(/314,275/)/), &
      (/(/315,237/),(/353,275/)/), &
      (/(/354,237/),(/392,275/)/), &
      (/(/393,237/),(/431,275/)/), &
      (/(/432,237/),(/470,275/)/), &
      (/(/471,237/),(/509,275/)/), &
      (/(/510,237/),(/549,275/)/), &
      (/(/550,237/),(/589,275/)/), &
      (/(/590,237/),(/628,275/)/), &
      (/(/  1,276/),(/ 40,314/)/), &
      (/(/ 41,276/),(/ 80,314/)/), &
      (/(/ 81,276/),(/119,314/)/), &
      (/(/120,276/),(/158,314/)/), &
      (/(/159,276/),(/197,314/)/), &
      (/(/198,276/),(/236,314/)/), &
      (/(/237,276/),(/275,314/)/), &
      (/(/276,276/),(/314,314/)/), &
      (/(/315,276/),(/353,314/)/), &
      (/(/354,276/),(/392,314/)/), &
      (/(/393,276/),(/431,314/)/), &
      (/(/432,276/),(/470,314/)/), &
      (/(/471,276/),(/509,314/)/), &
      (/(/510,276/),(/549,314/)/), &
      (/(/550,276/),(/589,314/)/), &
      (/(/590,276/),(/628,314/)/), &
      (/(/  1,315/),(/ 40,353/)/), &
      (/(/ 41,315/),(/ 80,353/)/), &
      (/(/ 81,315/),(/119,353/)/), &
      (/(/120,315/),(/158,353/)/), &
      (/(/159,315/),(/197,353/)/), &
      (/(/198,315/),(/236,353/)/), &
      (/(/237,315/),(/275,353/)/), &
      (/(/276,315/),(/314,353/)/), &
      (/(/315,315/),(/353,353/)/), &
      (/(/354,315/),(/392,353/)/), &
      (/(/393,315/),(/431,353/)/), &
      (/(/432,315/),(/470,353/)/), &
      (/(/471,315/),(/509,353/)/), &
      (/(/510,315/),(/549,353/)/), &
      (/(/550,315/),(/589,353/)/), &
      (/(/590,315/),(/628,353/)/), &
      (/(/  1,354/),(/ 40,392/)/), &
      (/(/ 41,354/),(/ 80,392/)/), &
      (/(/ 81,354/),(/119,392/)/), &
      (/(/120,354/),(/158,392/)/), &
      (/(/159,354/),(/197,392/)/), &
      (/(/198,354/),(/236,392/)/), &
      (/(/237,354/),(/275,392/)/), &
      (/(/276,354/),(/314,392/)/), &
      (/(/315,354/),(/353,392/)/), &
      (/(/354,354/),(/392,392/)/), &
      (/(/393,354/),(/431,392/)/), &
      (/(/432,354/),(/470,392/)/), &
      (/(/471,354/),(/509,392/)/), &
      (/(/510,354/),(/549,392/)/), &
      (/(/550,354/),(/589,392/)/), &
      (/(/590,354/),(/628,392/)/), &
      (/(/  1,393/),(/ 40,431/)/), &
      (/(/ 41,393/),(/ 80,431/)/), &
      (/(/ 81,393/),(/119,431/)/), &
      (/(/120,393/),(/158,431/)/), &
      (/(/159,393/),(/197,431/)/), &
      (/(/198,393/),(/236,431/)/), &
      (/(/237,393/),(/275,431/)/), &
      (/(/276,393/),(/314,431/)/), &
      (/(/315,393/),(/353,431/)/), &
      (/(/354,393/),(/392,431/)/), &
      (/(/393,393/),(/431,431/)/), &
      (/(/432,393/),(/470,431/)/), &
      (/(/471,393/),(/509,431/)/), &
      (/(/510,393/),(/549,431/)/), &
      (/(/550,393/),(/589,431/)/), &
      (/(/590,393/),(/628,431/)/), &
      (/(/  1,432/),(/ 40,470/)/), &
      (/(/ 41,432/),(/ 80,470/)/), &
      (/(/ 81,432/),(/119,470/)/), &
      (/(/120,432/),(/158,470/)/), &
      (/(/159,432/),(/197,470/)/), &
      (/(/198,432/),(/236,470/)/), &
      (/(/237,432/),(/275,470/)/), &
      (/(/276,432/),(/314,470/)/), &
      (/(/315,432/),(/353,470/)/), &
      (/(/354,432/),(/392,470/)/), &
      (/(/393,432/),(/431,470/)/), &
      (/(/432,432/),(/470,470/)/), &
      (/(/471,432/),(/509,470/)/), &
      (/(/510,432/),(/549,470/)/), &
      (/(/550,432/),(/589,470/)/), &
      (/(/590,432/),(/628,470/)/), &
      (/(/  1,471/),(/ 40,509/)/), &
      (/(/ 41,471/),(/ 80,509/)/), &
      (/(/ 81,471/),(/119,509/)/), &
      (/(/120,471/),(/158,509/)/), &
      (/(/159,471/),(/197,509/)/), &
      (/(/198,471/),(/236,509/)/), &
      (/(/237,471/),(/275,509/)/), &
      (/(/276,471/),(/314,509/)/), &
      (/(/315,471/),(/353,509/)/), &
      (/(/354,471/),(/392,509/)/), &
      (/(/393,471/),(/431,509/)/), &
      (/(/432,471/),(/470,509/)/), &
      (/(/471,471/),(/509,509/)/), &
      (/(/510,471/),(/549,509/)/), &
      (/(/550,471/),(/589,509/)/), &
      (/(/590,471/),(/628,509/)/), &
      (/(/  1,510/),(/ 40,549/)/), &
      (/(/ 41,510/),(/ 80,549/)/), &
      (/(/ 81,510/),(/119,549/)/), &
      (/(/120,510/),(/158,549/)/), &
      (/(/159,510/),(/197,549/)/), &
      (/(/198,510/),(/236,549/)/), &
      (/(/237,510/),(/275,549/)/), &
      (/(/276,510/),(/314,549/)/), &
      (/(/315,510/),(/353,549/)/), &
      (/(/354,510/),(/392,549/)/), &
      (/(/393,510/),(/431,549/)/), &
      (/(/432,510/),(/470,549/)/), &
      (/(/471,510/),(/509,549/)/), &
      (/(/510,510/),(/549,549/)/), &
      (/(/550,510/),(/589,549/)/), &
      (/(/590,510/),(/628,549/)/), &
      (/(/  1,550/),(/ 40,589/)/), &
      (/(/ 41,550/),(/ 80,589/)/), &
      (/(/ 81,550/),(/119,589/)/), &
      (/(/120,550/),(/158,589/)/), &
      (/(/159,550/),(/197,589/)/), &
      (/(/198,550/),(/236,589/)/), &
      (/(/237,550/),(/275,589/)/), &
      (/(/276,550/),(/314,589/)/), &
      (/(/315,550/),(/353,589/)/), &
      (/(/354,550/),(/392,589/)/), &
      (/(/393,550/),(/431,589/)/), &
      (/(/432,550/),(/470,589/)/), &
      (/(/471,550/),(/509,589/)/), &
      (/(/510,550/),(/549,589/)/), &
      (/(/550,550/),(/589,589/)/), &
      (/(/590,550/),(/628,589/)/), &
      (/(/  1,590/),(/ 40,628/)/), &
      (/(/ 41,590/),(/ 80,628/)/), &
      (/(/ 81,590/),(/119,628/)/), &
      (/(/120,590/),(/158,628/)/), &
      (/(/159,590/),(/197,628/)/), &
      (/(/198,590/),(/236,628/)/), &
      (/(/237,590/),(/275,628/)/), &
      (/(/276,590/),(/314,628/)/), &
      (/(/315,590/),(/353,628/)/), &
      (/(/354,590/),(/392,628/)/), &
      (/(/393,590/),(/431,628/)/), &
      (/(/432,590/),(/470,628/)/), &
      (/(/471,590/),(/509,628/)/), &
      (/(/510,590/),(/549,628/)/), &
      (/(/550,590/),(/589,628/)/), &
      (/(/590,590/),(/628,628/)/)  &
    /)
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(model, model_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p2"/), userRoutine=InitializeP2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! attach specializing method(s)
    call NUOPC_CompSpecialize(model, specLabel=model_label_SetClock, &
      specRoutine=SetClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP1(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS

    ! importable field: air_pressure_at_sea_level
    call NUOPC_Advertise(importState, &
      StandardName="air_pressure_at_sea_level", name="pmsl", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! importable field: surface_net_downward_shortwave_flux
    call NUOPC_Advertise(importState, &
      StandardName="surface_net_downward_shortwave_flux", name="rsns", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: sea_surface_temperature
    call NUOPC_Advertise(exportState, &
      StandardName="sea_surface_temperature", name="sst", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP2(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(len=ESMF_MAXSTR) :: cName
    logical                    :: syncdecomp
    character(len=ESMF_MAXSTR) :: logMsg
    type(ESMF_TimeInterval)    :: stabilityTimeStep
    type(ESMF_Field)           :: field
    type(ESMF_Grid)            :: gridIn
    type(ESMF_Grid)            :: gridOut
    type(ESMF_DistGrid)        :: distgrid
    
    rc = ESMF_SUCCESS

    call ESMF_GridCompGet(model, name=cName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call IsSyncDecomp(model, syncdecomp=syncdecomp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (syncdecomp) then
      write (logMsg,"(A,A)") trim(cName)//": ", &
        " model attribute SyncDecomp: true"
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
      distgrid = ESMF_DistGridCreate(minIndex=(/1, 1/), maxIndex=(/628, 628/), &
        deBlockList=blockList_sync, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      write (logMsg,"(A,A)") trim(cName)//": ", &
        " model attribute SyncDecomp: false"
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
      distgrid = ESMF_DistGridCreate(minIndex=(/1, 1/), maxIndex=(/628, 628/), &
        deBlockList=blockList_noSync, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! create a Grid object for Fields
    gridIn = ESMF_GridCreate(distgrid=distgrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    gridOut = gridIn ! for now out same as in

    call LogGrid(grid=gridIn, label="GRID_OCN" , rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: air_pressure_at_sea_level
    field = ESMF_FieldCreate(name="pmsl", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! importable field: surface_net_downward_shortwave_flux
    field = ESMF_FieldCreate(name="rsns", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: sea_surface_temperature
    field = ESMF_FieldCreate(name="sst", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine SetClock(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_TimeInterval)       :: stabilityTimeStep

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! initialize internal clock
    ! here: parent Clock and stability timeStep determine actual model timeStep
    !TODO: stabilityTimeStep should be read in from configuation
    !TODO: or computed from internal Grid information
    call ESMF_TimeIntervalSet(stabilityTimeStep, m=5, rc=rc) ! 5 minute steps
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetClock(model, clock, stabilityTimeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_State)              :: importState, exportState
    type(ESMF_Time)               :: currTime
    type(ESMF_TimeInterval)       :: timeStep

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep
    
    ! Because of the way that the internal Clock was set in SetClock(),
    ! its timeStep is likely smaller than the parent timeStep. As a consequence
    ! the time interval covered by a single parent timeStep will result in 
    ! multiple calls to the ModelAdvance() routine. Every time the currTime
    ! will come in by one internal timeStep advanced. This goes until the
    ! stopTime of the internal Clock has been reached.
    
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing OCN from: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_TimePrint(currTime + timeStep, &
      preString="--------------------------------> to: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine LogGrid(grid,label,rc)
! ! ARGUMENTS
    type(ESMF_Grid), intent(in)            :: grid
    character(len=*), intent(in), optional :: label
    integer, intent(out), optional         :: rc
! !DESCRIPTION:
!   Write ESMF grid information to PET logs
!   
!   The arguments are:                     
!   \begin{description}
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(len=64)           :: llabel
    character(len=64)           :: gridName
    type(ESMF_DistGrid)         :: distgrid
    character(len=64)           :: transferAction
    integer                     :: localDeCount
    integer                     :: dimCount, tileCount, deCount
    integer                     :: dimIndex, tileIndex, deIndex
    integer,allocatable         :: coordDimCount(:)
    integer                     :: coordDimMax
    integer,allocatable         :: minIndexPTile(:,:), maxIndexPTile(:,:)
    integer,allocatable         :: minIndexPDe(:,:), maxIndexPDe(:,:)
    integer                     :: stat
    character(len=ESMF_MAXSTR)  :: logMsg

    if (present(rc)) rc = ESMF_SUCCESS
    if (present(label)) then
      llabel = trim(label)
    else
      llabel = 'LogGrid'
    endif

    ! access localDeCount to show this is a real Grid
    call ESMF_GridGet(grid, name=gridName, &
      localDeCount=localDeCount, distgrid=distgrid, &
      dimCount=dimCount,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! allocate coordDim info accord. to dimCount and tileCount
    allocate(coordDimCount(dimCount), &
      stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of coordinate dimensions memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! get coordDim info
    call ESMF_GridGet(grid, coordDimCount=coordDimCount, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    coordDimMax = 0
    do dimIndex=1,dimCount
      coordDimMax = MAX(coordDimMax,coordDimCount(dimIndex))
    enddo

    if (coordDimMax == 1) then
      write (logMsg,"(A,A,A)") trim(llabel)//": ", &
        trim(gridName), &
        " is a rectilinear grid with 1D coordinates in each dimension."
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    endif

    deallocate(coordDimCount, &
      stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Dellocation of coordinate dimensions memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    write (logMsg,"(A,A,(A,I0))") trim(llabel)//": ", &
      trim(gridName), &
      " local decomposition count=",localDeCount
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)

    ! get dimCount and tileCount
    call ESMF_DistGridGet(distgrid, dimCount=dimCount, tileCount=tileCount, &
      deCount=deCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    write (logMsg,"(A,A,(A,I0))") trim(llabel)//": ", &
      trim(gridName), &
      " dimension count=",dimCount
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A,(A,I0))") trim(llabel)//": ", &
      trim(gridName), &
      " tile count=",tileCount
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A,(A,I0))") trim(llabel)//": ", &
      trim(gridName), &
      " decomp count=",deCount
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)

    ! allocate minIndexPTile and maxIndexPTile accord. to dimCount and tileCount
    allocate(minIndexPTile(dimCount, tileCount), &
      maxIndexPTile(dimCount, tileCount),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of index array memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! get minIndex and maxIndex arrays
    call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
       maxIndexPTile=maxIndexPTile, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    do tileIndex=1,tileCount
    do dimIndex=1,dimCount
      write (logMsg,"(A,A,A,4(I0,A))") trim(llabel)//": ", &
        trim(gridName), &       
        " (tile,dim,minIndexPTile,maxIndexPTile)=(", &
        tileIndex,",",dimIndex,",", &
        minIndexPTile(dimIndex,tileIndex),",", &
        maxIndexPTile(dimIndex,tileIndex),")"
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    enddo
    enddo

    deallocate(minIndexPTile, maxIndexPTile,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of index array memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! allocate minIndexPDe and maxIndexPDe accord. to dimCount and deCount
    allocate(minIndexPDe(dimCount, deCount), &
      maxIndexPDe(dimCount, deCount),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of index array memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! get minIndex and maxIndex arrays
    call ESMF_DistGridGet(distgrid, minIndexPDe=minIndexPDe, &
       maxIndexPDe=maxIndexPDe, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    do deIndex=1,deCount
    do dimIndex=1,dimCount
      write (logMsg,"(A,A,A,4(I0,A))") trim(llabel)//": ", &
        trim(gridName), &
        " (decomp,dim,minIndexPDe,maxIndexPDe)=(", &
        deIndex,",",dimIndex,",", &
        minIndexPDe(dimIndex,deIndex),",", &
        maxIndexPDe(dimIndex,deIndex),")"
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    enddo
    enddo

    deallocate(minIndexPDe, maxIndexPDe,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of index array memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine IsSyncDecomp(model, syncdecomp, rc)
    type(ESMF_GridComp)  :: model
    logical              :: syncdecomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR) :: attrString

    ! get MemCopy attribute
    call ESMF_AttributeGet(model, name='SyncDecomp', value=attrString, &
      defaultValue="true", convention='NUOPC', purpose='Instance', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    select case (attrString)
    case ('true','TRUE','True','t','T','1' )
      syncdecomp = .true.
    case default
      syncdecomp = .false.
    endselect

  end subroutine

  !-----------------------------------------------------------------------------

end module
