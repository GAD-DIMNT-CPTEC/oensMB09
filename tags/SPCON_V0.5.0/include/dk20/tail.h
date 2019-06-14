date
set +e
set +u
set +x
%include <logb.h>
set -e
set -u
set -x

smscomplete  # Notify SMS of a normal end
trap 0       # Remove all traps
exit 0       # End the shell
