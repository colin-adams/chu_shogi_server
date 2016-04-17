#include <stdint.h>
#include <microhttpd.h>

struct {int8_t connection_type;  char *answer_string; struct MHD_PostProcessor *post_processor; unsigned int code; char * login_name; char * email; char * password; int8_t guest; int8_t remember;} connection_information;
