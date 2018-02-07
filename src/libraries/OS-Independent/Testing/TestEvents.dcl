definition module Testing.TestEvents
/**
 * This module provides types representing events occurring during a test run.
 * Each Clean testing framework should report such events on StdOut, as JSON
 * representation of the event types provided here. The test runners
 * (https://gitlab.science.ru.nl/clean-and-itasks/clean-test) process the
 * events further.
 */

from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode, :: Maybe

/**
 * Event emitted when a test is started.
 * A Specialised JSONEncode instance is used for this type, which
 * has to be adapted in case the type definition is changed!
 */
:: StartEvent = { name    :: !String //* The test's name
                }
/**
 * Event emitted after a test has finished.
 */
:: EndEvent   = { name    :: !String       //* The test's name
                , event   :: !EndEventType //* The event's type, indicating success
                , message :: !String       //* Message providing an explanation for the result
                }

/**
 * Indicating the type an end event, indicating whether the test was
 * successful or not.
 * Specialised JSONEncode/JSONDecode instances are used for this type, which
 * have to be adapted in case the type definition is changed!
 */
:: EndEventType = Passed  //* The test passed
                | Failed  //* The test failed
                | Skipped //* The test was not executed, but should be executed and pass for future versions

derive JSONEncode StartEvent, EndEvent
derive JSONDecode StartEvent, EndEvent

