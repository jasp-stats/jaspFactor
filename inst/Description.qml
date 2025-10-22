import QtQuick
import JASP.Module

Description
{
	title		: qsTr("Factor")
	icon		: "analysis-classical-sem.svg"
	description	: qsTr("Explore hidden structure in the data")
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
		func:		"confirmatoryFactorAnalysis"
		qml:    "ConfirmatoryFactorAnalysis.qml"
	}
}
