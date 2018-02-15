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
 * Events that are emitted from tests.
 * Specialised JSONEncode/JSONDecode instances are used for this type, which
 * have to be adapted in case the type definition is changed!
 */
:: TestEvent
	= StartEvent StartEvent //* A test has started
	| EndEvent   EndEvent   //* A test has finished

/**
 * Event emitted when a test is started.
 * Specialised JSONEncode/JSONDecode instances are used for this type, which
 * have to be adapted in case the type definition is changed!
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
:: EndEventType = Passed            //* The test passed
                | Failed FailReason //* The test failed
                | Skipped           //* The test was not executed, but should be executed and pass for future versions
                | Lost              //* The test crashed

/**
 * Reasons for failing a test.
 */
:: FailReason
	= CounterExample JSONNode          //* A counter-example for a property was found
	| ExpectedButGot JSONNode JSONNode //* An equality test failed

derive JSONEncode TestEvent, StartEvent, EndEvent, FailReason
derive JSONDecode TestEvent, StartEvent, EndEvent, FailReason
