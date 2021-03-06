/*
 * Copyright © 2014 TU Berlin (emma@dima.tu-berlin.de)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.emmalanguage.mitos.operators;

import java.util.ArrayList;

public class IncMapSynced extends BagOperator<Integer,Integer> {

	private final ArrayList<Integer> buff = new ArrayList<>(1);

	@Override
	public void openOutBag() {
		super.openOutBag();
		buff.clear();
	}

	@Override
	public void pushInElement(Integer e, int logicalInputId) {
		super.pushInElement(e, logicalInputId);
		buff.add(e);
	}

	@Override
	public void closeInBag(int inputId) {
		super.closeInBag(inputId);
		for (Integer x: buff) {
			out.collectElement(x + 1);
		}
		out.closeBag();
	}
}
