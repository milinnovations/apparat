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

import apparat.utils.TagContainer
import apparat.actors.Futures._
import java.io.{File => JFile}
import apparat.tools.{ApparatConfiguration, ApparatTool, ApparatApplication}
import apparat.bytecode.operations._
import apparat.bytecode.combinator._
import apparat.bytecode.combinator.BytecodeChains._
import java.io.{File => JFile}
import apparat.abc.{AbcConstantPool, AbcQName, AbcNamespace, Abc}
import compat.Platform
import apparat.swf.{SwfTag, SwfTags, DoABC}

object Coverage {
	def main(args: Array[String]): Unit = ApparatApplication(new CoverageTool, args)

	class CoverageTool extends ApparatTool {
		val debugLine = partial { case DebugLine(line) => line }
		val coverageOnSample = AbcQName('onSample, AbcNamespace(22, Symbol("")))
		val coverageMethod = CallPropVoid(coverageOnSample, 3)

		var input: JFile = _
		var output: JFile = _
		var lineDump: JFile = _
		var sourcePath = List.empty[String]
		var coveragePackage: String = _
		var coverageQName: AbcQName = _
		var coverageScope: GetLex = _

		var observers = List.empty[MethodAwareCoverageObserver]

		override def name = "Coverage-ng"

		override def help = """  -i [file]	Input file
  -o [file]	Output file (optional)
  -d [file]	File containing the line numbers of all the executable lines (optional)
  -p [str]	Name of the package containing the Coverage class (optional, defaults to apparat.coverage).
  -s [dir]	Source path to instrument"""

		override def configure(config: ApparatConfiguration): Unit = configure(CoverageConfigurationFactory fromConfiguration config)

		def configure(config: CoverageConfiguration): Unit = {
			input = config.input
			output = config.output
			lineDump = config.lineDump
			sourcePath = config.sourcePath
			coveragePackage = config.coveragePackage
		}

		override def run() = {
			coverageQName = AbcQName('Coverage, AbcNamespace(22, Symbol(coveragePackage)))
			coverageScope = GetLex(coverageQName)

			SwfTags.tagFactory = (kind: Int) => kind match {
				case SwfTags.DoABC => Some(new DoABC)
				case SwfTags.DoABC1 => Some(new DoABC)
				case _ => None
			}

			val lineDumpObserver = Option(lineDump).map(new DumpExecutableLinesObserver(_))
			lineDumpObserver foreach (x => addObserver(x))

			val cont = TagContainer fromFile input
			cont foreachTagSync coverage
			cont write output

			lineDumpObserver foreach (x => x.terminate)
		}

		def addObserver(observer: MethodAwareCoverageObserver) = {
			observers = observer :: observers
		}

		def removeObserver(observer: MethodAwareCoverageObserver) = {
			observers = observers filterNot (_ == observer)
		}

		private def coverage: PartialFunction[SwfTag, Unit] = {
			case doABC: DoABC => {
				var abcModified = false
				var previousLine = -1
				val abc = Abc fromDoABC doABC

				abc.loadBytecode()

				for {
					method <- abc.methods
					body <- method.body
					bytecode <- body.bytecode
				} {
					bytecode.ops find (_.opCode == Op.debugfile) match {
						case Some(op) => {
							val debugFile = op.asInstanceOf[DebugFile]
							val file = debugFile.file
							if(sourcePath.isEmpty || (sourcePath exists (file.name startsWith _))) {
								abcModified = true

								bytecode.replaceFrom(4, debugLine) {
									x =>
										observers foreach (_.instrument(file.name, method.name.name, x))
										if (previousLine != x) {
											previousLine = x
											DebugLine(x) ::
											coverageScope ::
											PushString(file) ::
											PushString(method.name) ::
											pushLine(x) ::
											coverageMethod :: Nil
										} else {
											DebugLine(x) :: Nil
										}
								}

								body.maxStack += 4
							}
						}
						case None =>
					}
				}

				if(abcModified) {
					abc.cpool = (abc.cpool add coverageQName) add coverageOnSample
					abc.saveBytecode()
					abc write doABC
				}
			}
		}

		private def pushLine(line: Int) = line match {
			case x if x < 0x80 => PushByte(x)
			case x if x < 0x8000 => PushShort(x)
			case x => error("Too many lines.")
		}
	}
}
