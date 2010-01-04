definition module PMForms

import PMHtml, PMDataModel
import HTTP, Map

showProjectForm		:: Project -> HtmlTag
editProjectForm		:: Bool Project [ProjectID] [EmployeeID] -> HtmlTag
editProjectUpd		:: (Map String String) -> Project

showEmployeeForm	:: Employee -> HtmlTag
editEmployeeForm	:: Bool Employee [ProjectID] -> HtmlTag
editEmployeeUpd		::(Map String String) -> Employee
