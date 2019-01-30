#include <stdio.h>
#include <uuid/uuid.h>

// Generate human readable uuid
void gen_uuid (char *uuid_str)
{
	// Declare variables
	uuid_t uuid;

	// Real uuid generation with libuuid
	uuid_generate(uuid);

    uuid_unparse_lower(uuid, uuid_str);
}
