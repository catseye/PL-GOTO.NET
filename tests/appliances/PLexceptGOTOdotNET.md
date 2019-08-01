This file contains only the [Falderal][] directives that define the different
functionalities tested by the test suite, as provided by the reference
implementation.

[Falderal]:     http://catseye.tc/node/Falderal

    -> Functionality "Parse PL-{GOTO} Program" is implemented by
    -> shell command
    -> "bin/PLexceptGOTOdotNET parse %(test-body-file)"

    -> Functionality "Label PL-{GOTO} Loops" is implemented by
    -> shell command
    -> "bin/PLexceptGOTOdotNET labelloops %(test-body-file)"

    -> Functionality "Evaluate PL-{GOTO} Program" is implemented by
    -> shell command
    -> "bin/PLexceptGOTOdotNET interpret %(test-body-file)"
