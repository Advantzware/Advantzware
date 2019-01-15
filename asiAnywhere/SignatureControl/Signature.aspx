<%@ Page Language="C#" %>
<%@ Import Namespace="System.Web" %>
<%@ Import Namespace="System.Drawing" %>
<%@ Import Namespace="System.Drawing.Drawing2D" %>
<%@ Import Namespace="System.Drawing.Imaging" %>
<%@ Import Namespace="System.Data.SqlClient" %>
<%@ Import Namespace="System.Data" %>
<!-- 
	WebSignatureCapture (WEBSIGN) copyright © 2008 - 2009 www.websignaturecapture.com
	Contact: info@websignaturecapture.com
	This code is not a freeware. You are not authorized to distribute
	or use it if you have not purchased. Please visit
	http://www.websignaturecapture.com to buy it
-->
<html>
  <head>
    <title>Signature</title>
<style>
	body{
		background:#fff;
		cursor : hand;
		padding:0;
		margin:0;
		width:100%;
		height:100%;
	}
	#pointer{
		position:absolute;
		background:#000;
		width:3px;
		height:3px;
		font-size:1px;
		z-index:32768;
	}
</style>
    <script language=javascript>
			if (self == top) { 
			 window.location.href = './';
			 }
    </script>
  </head>
  <body>
	<noscript>
	    <META http-equiv="refresh" content="1;URL=./"> 
    </noscript>
    <form id="Form1" method="post" runat="server">
		
		<script runat="server">
	        
				string tmp1 = "";
				string tmp2 = "";
				string pcolor = "";
				string pwidth = "";
                string bgcolor = "";
				string signaturefile = "";
				string cWidth = "";
				string cHeight = "";
				string sSavePath = "";

		        // fix for IE bug of session variables in iframe    
                protected override void OnPreRender(EventArgs e)
                {
                    Response.AppendHeader("P3P", "CP=\"CAO PSA OUR\"");
                    base.OnPreRender(e);
                }
		        
				void Page_Load(object sender, System.EventArgs e)
				{
					if(!Page.IsPostBack)
					{
					   try
					   {
						tmp1 = Request.Form[0];
						tmp2 = Request.Form[1];
						pwidth = Request.Form[2];
						pcolor = Request.Form[3];
                        bgcolor = Request.Form[4];
                        signaturefile = Request.Form[5];
						cWidth = Request.Form[6];
						cHeight = Request.Form[7];
                        sSavePath = Convert.ToString(Request.Form[8]).Replace("_", "/");
					   }
					   catch (Exception)
					   {
					     Response.Redirect(Page.ResolveUrl("~/"));
					   }
					   
 					   GenerateImage();
							
					}
				}
				
				void GenerateImage()
				{
				    Response.Clear();
					string[] arrX = tmp1.Split(',');
					string[] arrY = tmp2.Split(',');

					int CurrX = 0;
					int CurrY = 0;

					int NextX = 0;
					int NextY = 0;

					int currCount = 0;

					bool isErr = false;

					Bitmap bmp = new Bitmap(Convert.ToInt32(cWidth), Convert.ToInt32(cHeight) , PixelFormat.Format24bppRgb);
					Graphics g = null;
		            
					try
					{
						g = Graphics.FromImage(bmp);

						g.FillRectangle(new SolidBrush(Color.FromName(bgcolor)),0,0,bmp.Width,bmp.Height);

						Pen pn = new Pen(new SolidBrush(Color.FromName(pcolor)), Convert.ToInt32(pwidth));

						for(int i = 0;i <= arrX.Length - 4;i++)
						{

							if(IsNumeric(arrX[i], arrY[i], arrX[i + 1], arrY[i + 1]))
							{
								CurrX = Convert.ToInt32(arrX[i]);
								CurrY = Convert.ToInt32(arrY[i]);

								currCount = i;
		                    
								NextX = Convert.ToInt32(arrX[i + 1]);
								NextY = Convert.ToInt32(arrY[i + 1]);

								g.DrawLine(pn, CurrX, CurrY, NextX, NextY);
							}
						}

					}
					catch(Exception ex)
					{
						string err = ex.Message.ToString();
						isErr = true;
					}
					finally
					{
						if(null != g)
							g.Dispose();
					}

                    string formatExt = signaturefile.Split('.')[1];

                    try
                    {
                        ImageFormat imgFormat = ParseImageFormat(formatExt);
                        string outPath = Server.MapPath(sSavePath) + "/" + Convert.ToString(Session["signaturefile_image_name"]);

                        bmp.Save(outPath, imgFormat);

                        if (Color.FromName(bgcolor) == Color.White && imgFormat == ImageFormat.Gif)
                        {
                            MakeTransparent(outPath);
                        }

                        Response.ContentType = "image/" + formatExt;
                        bmp.Save(Response.OutputStream, imgFormat);

                        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                        try
                        {
                        conn.Open();

                        string cmd = "select * from sign_bol where cust = '" + Convert.ToString(Session["signatures_bill_oflading_cust"]) + "' and bol = '" + Convert.ToInt32(Session["signatures_bill_oflading_bol"]) + "' ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);

                        if (ds.Tables[0].Rows.Count == 0)
                        {
                            SqlCommand cmd_insert = new SqlCommand("insert into sign_bol (cust, bol, imagepath, pdfname, pdfsigname, flag) values ('" + Convert.ToString(Session["signatures_bill_oflading_cust"]) + "','" + Convert.ToInt32(Session["signatures_bill_oflading_bol"]) + "','" + "Signatures/" +Convert.ToString(Session["signaturefile_image_name"]) + "','" + "" + "' ,'" + "" + "','0')", conn);
                            cmd_insert.ExecuteNonQuery();
                        }
                        else
                        {
                            SqlCommand cmd_update = new SqlCommand("update sign_bol set imagepath = '" + "Signatures/" +Convert.ToString(Session["signaturefile_image_name"]) + "', flag = '0' where cust = '" + Convert.ToString(Session["signatures_bill_oflading_cust"]) + "' and bol = '" + Convert.ToInt32(Session["signatures_bill_oflading_bol"]) + "' ", conn);
                            cmd_update.ExecuteNonQuery();
                        }
                        conn.Close();

                        }
                        catch
                        {

                            conn.Close();
                        }

                        // dispose bitmap object 
                        bmp.Dispose();
                        Response.End();
                    }
                    catch (Exception ex)
                    {
                        string err = ex.Message.ToString();
                        Response.Write(err);
                        isErr = true;
                    }
                    
                                      

				}

                private Bitmap MakeTransparent(string outPath)
                {
                    Bitmap bmpIn = new Bitmap(outPath);
                    
                    try
                    {
                        ImageAttributes mImageAttributes = new ImageAttributes();
                        mImageAttributes.SetColorKey(bmpIn.GetPixel(0, 0), bmpIn.GetPixel(0, 0));
                        Rectangle dstRect = new Rectangle(0, 0, bmpIn.Width, bmpIn.Height);
                        
                        Bitmap bmnew = new Bitmap(bmpIn.Width, bmpIn.Height);
                        Graphics g = Graphics.FromImage(bmnew);
                        g.DrawImage(bmpIn, dstRect, 0, 0, bmpIn.Width, bmpIn.Height, GraphicsUnit.Pixel, mImageAttributes);

                        bmpIn.Dispose();
                        System.IO.File.Delete(outPath);
                        bmnew.Save(outPath);
                        return bmnew;
                    }
                    catch(Exception ex)
                    {
                        string transErr = ex.Message.ToString();
                    }
                    return bmpIn;
                }    

				bool IsNumeric(string str1, string str2, string str3, string str4)
				{
					try
					{
						int i = Convert.ToInt32(str1);
						i = Convert.ToInt32(str2);
						i = Convert.ToInt32(str3);
						i = Convert.ToInt32(str4);
						return true;
					}
					catch
					{
						return false;
					}
				}

            private ImageFormat ParseImageFormat(string format)
            {
                switch (format.ToLower())
                {
                    case "jpg":
                        return ImageFormat.Jpeg;
                    case "jpeg":
                        return ImageFormat.Jpeg;
                    case "bmp":
                        return ImageFormat.Bmp;
                    case "gif":
                        return ImageFormat.Gif;
                    case "png":
                        return ImageFormat.Png;
                    case "tiff":
                        return ImageFormat.Tiff;
                    case "wmf":
                        return ImageFormat.Wmf;
                    case "emf":
                        return ImageFormat.Emf;
                    case "icon":
                        return ImageFormat.Icon;
                    case "ico":
                        return ImageFormat.Icon;
                    case "exif":
                        return ImageFormat.Exif;
                    default:
                        return ImageFormat.Jpeg;
                }

            }
        
        </script>    
     </form>
	
  </body>
</html>
