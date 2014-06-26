/*
 * This file is part of Apparat.
 *
 * Copyright (C) 2014 Tamas Nepusz
 *
 * Apparat is copyright (C) 2010 Joa Ebert
 * http://www.joa-ebert.com/
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */
package apparat.tools.coverage2

import collection.mutable.{HashMap, TreeSet}
import java.io.{File => JFile, PrintWriter => JPrintWriter}

/**
 * @author Tamas Nepusz
 */
protected[coverage2] class DumpExecutableLinesObserver(
	val output: JFile) extends MethodAwareCoverageObserver {

	private val outStream = new JPrintWriter(output)
	private val lineMap = new HashMap[String, TreeSet[Int]]

	def instrument(file: String, method:String, line: Int): Unit = {
		val lineSet = lineMap getOrElseUpdate(file + ";" + method, new TreeSet[Int])
		lineSet += line
	}

	def terminate(): Unit = {
		for ((fileAndMethod, lines) <- lineMap.toList sortBy {_._1}) {
			outStream.println(fileAndMethod + "\t" + lines.mkString("\t"))
    }
		outStream.close
	}
}

