/*
 * This file is part of Apparat.
 *
 * Copyright (C) 2010 Joa Ebert
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

import java.io.{File => JFile}

/**
 * @author Joa Ebert
 */
trait CoverageConfiguration {
	/**
	 * The input file.
	 */
	def input: JFile

	/**
	 * The output file.
	 */
	def output: JFile

	/**
	 * The file that will contain the line numbers of all the lines containing
	 * executable code.
	 */
	def lineDump: JFile

	/**
	 * The source-path to instrument.
	 *
	 * This list must include the absolute path to the directories containing
	 * the ActionScript classes that should be instrumented.
	 */
	def sourcePath: List[String]

	/**
	 * The name of the package that contains the Coverage class.
	 */
	def coveragePackage: String
}
