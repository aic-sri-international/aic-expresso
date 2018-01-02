/*
 * Copyright (c) 2013, SRI International
 * All rights reserved.
 * Licensed under the The BSD 3-Clause License;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at:
 * 
 * http://opensource.org/licenses/BSD-3-Clause
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the aic-expresso nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.sri.ai.grinder.sgdpllt.core.solver;

import static com.sri.ai.util.Util.incrementValue;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;

import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionStepSolver.Step;
import com.sri.ai.grinder.sgdpllt.api.SingleQuantifierEliminationProblem;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;
import com.sri.ai.util.Util;

@Beta
public class IntegrationRecording {

	private static boolean recordingIntegrationsOverGroups = false;
	private static boolean storingIntegrationsOverGroups = false;
	private static Map<AssociativeCommutativeGroup, Integer> numberOfIntegrationsOverGroup = map();
	private static Map<AssociativeCommutativeGroup, List<Integration>> integrationsOverGroup = map();
	
	public static void startRecordingIntegrationsOverGroups() {
		resetRecordingIntegrationsOverGroups();
		turnRecordingIntegrationsOverGroupsOn();
	}
	
	public static void resetRecordingIntegrationsOverGroups() {
		integrationsOverGroup = map();
		numberOfIntegrationsOverGroup = map();
	}
	
	public static void turnRecordingIntegrationsOverGroupsOn() {
		recordingIntegrationsOverGroups = true;
		storingIntegrationsOverGroups = true;
	}
	
	public static void turnRecordingIntegrationsOverGroupsOff() {
		recordingIntegrationsOverGroups = false;
		storingIntegrationsOverGroups = false;
	}
	
	public static boolean isRecordingIntegrationsOverGroups() {
		return recordingIntegrationsOverGroups;
	}
	
	public static void turnStoringIntegrationsOverGroupsOn() {
		storingIntegrationsOverGroups = true;
	}
	
	public static void turnStoringIntegrationsOverGroupsOff() {
		storingIntegrationsOverGroups = false;
	}
	
	public static boolean isStoringIntegrationsOverGroups() {
		return storingIntegrationsOverGroups;
	}
	
	public static List<Integration> getIntegrationsOverGroup(AssociativeCommutativeGroup group) {
		List<Integration> result;
		if (storingIntegrationsOverGroups) {
			result = integrationsOverGroup.get(group);
		}
		else {
			result = null;
		}
		
		if (result == null) {
			result = list();
		}
		
		return result;
	}

	public static int getNumberOfIntegrationsOverGroup(AssociativeCommutativeGroup group) {
		Integer value = numberOfIntegrationsOverGroup.get(group);
		int result = value == null? 0 : value.intValue();
		return result;
	}

	public static void registerGroupIntegration(SingleQuantifierEliminationProblem problem, Expression literalFreeBody, Step result, Context context) {
		if (recordingIntegrationsOverGroups) {
			store(problem, literalFreeBody, context, result);
			incrementValue(numberOfIntegrationsOverGroup, problem.getGroup());
		}
	}

	private static void store(SingleQuantifierEliminationProblem problem, Expression literalFreeBody, Context context, Step result) {
		if (storingIntegrationsOverGroups) {
			SingleQuantifierEliminationProblem problemWithNewBody = problem.makeWithNewBody(literalFreeBody);
			Integration integration = new Integration(problem, problemWithNewBody, context, result);
			Util.putInListValue(integrationsOverGroup, problem.getGroup(), integration);
		}
	}
}