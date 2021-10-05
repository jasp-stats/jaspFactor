import QtQuick 		2.12
import JASP.Module 	1.0

Upgrades
{

	Upgrade
	{
		functionName:       "PrincipalComponentAnalysis"
		fromVersion:		"0.16"
		toVersion:			"0.17"

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
		fromVersion:		"0.16"
		toVersion:			"0.17"

		ChangeSetValue
		{
			condition:	function(options) { return options["parallelMethod"] === undefined }
			name:		"parallelMethod"
			jsonValue:	"fa"
		}
	}
}
