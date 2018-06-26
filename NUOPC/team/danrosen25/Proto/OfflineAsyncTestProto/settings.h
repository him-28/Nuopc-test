#define DEFAULT_LOGKIND  ESMF_LOGKIND_NONE
#define DEFAULT_TYPEKIND ESMF_TYPEKIND_R4
#define DEFAULT_CONFIG "runconfig.default"
#define DEFAULT_ROOT   0
#define DEFAULT_X    628
#define DEFAULT_Y    628
#define DEFAULT_WRTESMF  .false.
#define DEFAULT_PARALLEL .false.
#define DEFAULT_ASYNC    .false.

#ifdef ESMF_TRACE
#define T_ENTER(region) call ESMF_TraceRegionEnter(region)
#define T_EXIT(region) call ESMF_TraceRegionExit(region)
#else
#define T_ENTER(region) 
#define T_EXIT(region)
#endif
