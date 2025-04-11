import QtQuick
import JASP.Module

Description
{
	name		: "jaspFactor"
	title		: qsTr("Factor")
	icon		: "analysis-classical-sem.svg"
	description	: qsTr("Explore hidden structure in the data")
<<<<<<< HEAD
	version			: "0.20.0"
=======
	version			: "0.19.3"
>>>>>>> f0fbe94 (update renv)
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "jasp-stats.org"
	license		: "GPL (>= 2)"
	hasWrappers	: true
	preloadData	: true

	Analysis
	{
		title:	qsTr("Principal Component Analysis")
		func:	"principalComponentAnalysis"
		qml:	"PrincipalComponentAnalysis.qml"
	}

	Analysis
	{
		title:	qsTr("Exploratory Factor Analysis")
		func:	"exploratoryFactorAnalysis"
		qml:	"ExploratoryFactorAnalysis.qml"
	}

	Analysis
	{
		title:	qsTr("Confirmatory Factor Analysis")
		func:		"confirmatoryFactorAnalysis"
		qml:    "ConfirmatoryFactorAnalysis.qml"
	}
}
