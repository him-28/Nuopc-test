/* -------------------------------------------------------------------------- */
/* NAVY ESPC CPP Macros                                                       */
/* -------------------------------------------------------------------------- */


/* -------------------------------------------------------------------------- */
/* Macros for ESMF logging                                                    */
/* -------------------------------------------------------------------------- */
#ifndef FILENAME
#define FILENAME __FILE__
#endif
#define CONTEXT  line=__LINE__,file=FILENAME
#define PASSTHRU msg=ESMF_LOGERR_PASSTHRU,CONTEXT


/* -------------------------------------------------------------------------- */
/* Define real kind for data passed through ESMF interface                    */
/* -------------------------------------------------------------------------- */
#if defined(REAL8)
#define _ESMF_KIND_RX _ESMF_KIND_R8
#define ESMF_KIND_RX ESMF_KIND_R8
#define ESMF_TYPEKIND_RX ESMF_TYPEKIND_R8
#else
#define _ESMF_KIND_RX _ESMF_KIND_R4
#define ESMF_KIND_RX ESMF_KIND_R4
#define ESMF_TYPEKIND_RX ESMF_TYPEKIND_R4
#endif


/* -------------------------------------------------------------------------- */
/* Define macros for mask values                                              */
/* -------------------------------------------------------------------------- */
#define NESPC_MASK_INLAND_WATER -1
#define NESPC_MASK_WATER         0
#define NESPC_MASK_LAND          1
#define NESPC_MASK_FROZEN_WATER  2
#define NESPC_MASK_FROZEN_LAND   3
