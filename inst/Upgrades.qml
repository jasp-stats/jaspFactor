import QtQuick 		2.12
import JASP.Module 	1.0

Upgrades
{

	Upgrade
	{
		functionName:       "PrincipalComponentAnalysis"
		newFunctionName:	"principalComponentAnalysis"
		fromVersion:		"0.15"
		toVersion:			"0.16.4"

		ChangeSetValue
		{
			condition:	function(options) { return options["parallelMethod"] === undefined }
			name:		"parallelMethod"
			jsonValue:	"pc"
		}
	}


	Upgrade
	{
		functionName:       "ExploratoryFactorAnalysis"
		newFunctionName:	"exploratoryFactorAnalysis"
		fromVersion:		"0.15"
		toVersion:			"0.16.4"

		ChangeSetValue
		{
			condition:	function(options) { return options["parallelMethod"] === undefined }
			name:		"parallelMethod"
			jsonValue:	"fa"
		}
	}
}
