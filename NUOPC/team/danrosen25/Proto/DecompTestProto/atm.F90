    ! Disabling the following macro, e.g. renaming to DECOMPSYNC_disable,
    ! will result in a model component that desynchronizes the domain
    ! decomposition.
#define DECOMPSYNC
    ! Disabling the following macro, e.g. renaming to WITHIMPORTFIELDS_disable,
    ! will result in a model component that does not advertise any importable
    ! Fields. Use this if you want to drive the model independently.
#define WITHIMPORTFIELDS

module ATM

  !-----------------------------------------------------------------------------
  ! ATM Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS    => SetServices, &
    model_label_Advance => label_Advance
  
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

#ifdef WITHIMPORTFIELDS
    ! importable field: sea_surface_temperature
    call NUOPC_Advertise(importState, &
      StandardName="sea_surface_temperature", name="sst", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
    
    ! exportable field: air_pressure_at_sea_level
    call NUOPC_Advertise(exportState, &
      StandardName="air_pressure_at_sea_level", name="pmsl", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! exportable field: surface_net_downward_shortwave_flux
    call NUOPC_Advertise(exportState, &
      StandardName="surface_net_downward_shortwave_flux", name="rsns", rc=rc)
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
    type(ESMF_Field)        :: field
    type(ESMF_DistGrid)     :: distGrid
    type(ESMF_Grid)         :: gridIn
    type(ESMF_Grid)         :: gridOut
    
    rc = ESMF_SUCCESS
 
#ifdef DECOMPSYNC   
    distGrid = ESMF_DistGridCreate(minIndex=(/1, 1/), maxIndex=(/628, 628/), &
      deBlockList=blockList_sync, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#else
    distGrid = ESMF_DistGridCreate(minIndex=(/1, 1/), maxIndex=(/628, 628/), &
      deBlockList=blockList_noSync, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    ! create a Grid object for Fields
    gridIn = ESMF_GridCreate(distgrid=distgrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    gridOut = gridIn ! for now out same as in

    call LogGrid(grid=gridIn, label="GRID_ATM" , rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#ifdef WITHIMPORTFIELDS
    ! importable field: sea_surface_temperature
    field = ESMF_FieldCreate(name="sst", grid=gridIn, &
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
#endif

    ! exportable field: air_pressure_at_sea_level
    field = ESMF_FieldCreate(name="pmsl", grid=gridOut, &
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

    ! exportable field: surface_net_downward_shortwave_flux
    field = ESMF_FieldCreate(name="rsns", grid=gridOut, &
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

  subroutine ModelAdvance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_State)              :: importState, exportState

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep
    
    ! Because of the way that the internal Clock was set by default,
    ! its timeStep is equal to the parent timeStep. As a consequence the
    ! currTime + timeStep is equal to the stopTime of the internal Clock
    ! for this call of the ModelAdvance() routine.
    
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing ATM from: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_ClockPrint(clock, options="stopTime", &
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

end module
