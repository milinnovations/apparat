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
package apparat.swc

import java.io.{
	BufferedInputStream => JBufferedInputStream,
	File => JFile,
	FileInputStream => JFileInputStream,
	FileOutputStream => JFileOutputStream,
	ByteArrayInputStream => JByteArrayInputStream,
	InputStream => JInputStream,
	OutputStream => JOutputStream
}
import java.util.zip.{
	Deflater => JDeflater,
	ZipInputStream => JZipInputStream,
	ZipOutputStream => JZipOutputStream,
	ZipEntry => JZipEntry
}
import apparat.utils.IO._
import apparat.utils.IO

object Swc {
	def fromFile(file: JFile): Swc = {
		val swc = new Swc
		swc read file
		swc
	}

	def fromFile(pathname: String): Swc = fromFile(new JFile(pathname))

	def fromInputStream(input: JInputStream) = {
		val swc = new Swc
		swc read input
		swc
	}
}

class Swc {
	var catalog: Option[Array[Byte]] = None
	var library: Option[Array[Byte]] = None
	var unknown: Map[String, Array[Byte]] = Map.empty

	def read(file: JFile): Unit = using(new JBufferedInputStream(new JFileInputStream(file), 0x1000))(read _)

	def read(pathname: String): Unit = read(new JFile(pathname))

	def read(input: JInputStream): Unit = using(new JZipInputStream(input))(read _)

	def read(data: Array[Byte]): Unit = using(new JByteArrayInputStream(data))(read _)

	def read(input: JZipInputStream): Unit = extract(input)

	def write(file: JFile): Unit = using(new JFileOutputStream(file))(write _)

	def write(pathname: String): Unit = write(new JFile(pathname))

	def write(output: JOutputStream): Unit = {
		val zipOutput = new JZipOutputStream(output)
		try {
			zipOutput setMethod JZipOutputStream.DEFLATED
			zipOutput setLevel JDeflater.BEST_COMPRESSION

			write(zipOutput)

			zipOutput flush ()
			zipOutput close ()
		} finally {
			try {zipOutput close ()} catch {case _:Throwable => {}}
		}
	}

	def write(output: JZipOutputStream): Unit = {
		write(output, catalog, "catalog.xml")
		write(output, library, "library.swf")

		for((name, bytes) <- unknown) {
			write(output, Some(bytes), name)
		}

		output flush ()
	}

	private def write(output: JZipOutputStream, target: Option[Array[Byte]], name: String): Unit = target match {
		case Some(data) => {
			output putNextEntry new JZipEntry(name)
			output write data
			output closeEntry
		}
		case None => error(name + " is missing.")
	}

	private def extract(implicit input: JZipInputStream): Unit = input getNextEntry match {
		case null => {}
		case entry => {
			if(entry.isDirectory) {
				extract(input)
			} else {
				val name = entry.getName()
				val size = entry.getSize()

				name match {
					case "catalog.xml" => catalog = Some(extractBytes(size.asInstanceOf[Int]))
					case "library.swf" => library = Some(extractBytes(size.asInstanceOf[Int]))
					case other => unknown += other -> extractBytes(size.asInstanceOf[Int])
				}

				extract(input)
			}
		}
	}

	private def extractBytes(length: Int)(implicit input: JZipInputStream) = length match {
		case -1 => byteArrayOf(input)
		case _ => IO read length
	}
}
