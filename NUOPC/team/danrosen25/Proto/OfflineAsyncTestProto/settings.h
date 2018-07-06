#define DEFAULT_LOGKIND      ESMF_LOGKIND_NONE
#define DEFAULT_TYPEKIND     ESMF_TYPEKIND_R4
#define DEFAULT_MISSING      999999.0
#define DEFAULT_KIND         ESMF_KIND_R4
#define DEFAULT_NC_PAR       .false.
#define DEFAULT_NC_PARTYPE   IOR(NF90_NETCDF4,NF90_MPIIO)
#define DEFAULT_NC_PARACCESS NF90_COLLECTIVE
#define DEFAULT_ESMF_FBWRT   .false.
#define DEFAULT_ESMF_ASYNC   .false.
#define DEFAULT_ROOT         0
#define DEFAULT_CONFIG       "runconfig.default"
#define DEFAULT_X            628
#define DEFAULT_Y            628

#ifdef ESMF_TRACE
#define T_ENTER(region) call ESMF_TraceRegionEnter(region)
#define T_EXIT(region)  call ESMF_TraceRegionExit(region)
#else
#define T_ENTER(region) 
#define T_EXIT(region)  
#endif
