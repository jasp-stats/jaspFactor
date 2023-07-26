import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name		: "jaspFactor"
	title		: qsTr("Factor")
	icon		: "analysis-classical-sem.svg"
	description	: qsTr("Explore hidden structure in the data")
	version			: "0.18.0"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "jasp-stats.org"
	license		: "GPL (>= 2)"
	hasWrappers	: true

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
		func:	"confirmatoryFactorAnalysis"
		qml:    "ConfirmatoryFactorAnalysis.qml"
	}
}
