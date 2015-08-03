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

import apparat.tools.{ApparatConfiguration, ApparatConfigurationFactory}
import java.io.{File => JFile}

/**
 * @author Joa Ebert
 */
object CoverageConfigurationFactory extends ApparatConfigurationFactory[CoverageConfiguration] {
	override def fromConfiguration(config: ApparatConfiguration): CoverageConfiguration = {
		val input = new JFile(config("-i") getOrElse error("Input is required."))
		val output = config("-o") map { pathname => new JFile(pathname) } getOrElse input
		val lineDump = config("-d") map { pathname => new JFile(pathname) } getOrElse null
		val sourcePath = config("-s") map { _ split JFile.pathSeparatorChar toList } getOrElse List.empty[String]
		val coveragePackage = config("-p") getOrElse "apparat.coverage"

		if(!input.exists) {
			error("Input "+input+" does not exist.")
		}

		new CoverageConfigurationImpl(input, output, lineDump, sourcePath, coveragePackage)
	}
}
