package flowanalyser.tests;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;

public class All_Tests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Tests for SPSC");
		// $JUnit-BEGIN$
		suite.addTest(new JUnit4TestAdapter(flowanalyser.tests.FlowAnalyser_Tests.class));
		// $JUnit-END$
		return suite;
	}

}