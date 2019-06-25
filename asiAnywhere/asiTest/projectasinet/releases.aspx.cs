using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Net.Mail;
using System.Data.SqlClient;
using System.Linq;

public partial class releases : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        if (Session["view_order_entry_pages_with_estimate"] == null)
        {
            /*ImageButton img4 = (ImageButton)Master.FindControl("ImageButton4");
            ImageButton img5 = (ImageButton)Master.FindControl("ImageButton5");
            ImageButton img6 = (ImageButton)Master.FindControl("ImageButton6");
            ImageButton img7 = (ImageButton)Master.FindControl("ImageButton7");
            img4.Visible = false;
            img5.Visible = false;
            img6.Visible = false;
            img7.Visible = false;

            Image ack = (Image)Master.FindControl("Image2");
            ImageButton add = (ImageButton)Master.FindControl("img_btn_add");
            ack.Visible = false;
            add.Visible = false;*/
            HtmlGenericControl img4 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton4");
            HtmlGenericControl img5 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton5");
            HtmlGenericControl img6 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton6");
            HtmlGenericControl img7 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton7");
            img4.Attributes.Add("style", "display:none");
            img5.Attributes.Add("style", "display:none");
            img6.Attributes.Add("style", "display:none");
            img7.Attributes.Add("style", "display:none");
        }

        if (Session["view_order_entry_pages_with_estimate"] != null)
        {
            /*ImageButton img1 = (ImageButton)Master.FindControl("brwsorder");
            ImageButton img2 = (ImageButton)Master.FindControl("vieworder");
            ImageButton img3 = (ImageButton)Master.FindControl("viewitem");

            img1.Visible = false;
            img2.Visible = false;
            img3.Visible = false;

            ImageButton img4 = (ImageButton)Master.FindControl("ImageButton1");
            ImageButton img5 = (ImageButton)Master.FindControl("ImageButton2");
            ImageButton img6 = (ImageButton)Master.FindControl("ImageButton3");

            ImageButton img11 = (ImageButton)Master.FindControl("listitem");
            img4.Visible = false;
            img5.Visible = false;
            img6.Visible = false;

            img11.Visible = false;*/
            HtmlGenericControl img1 = (HtmlGenericControl)this.Page.Master.FindControl("librowseorder");
            HtmlGenericControl img2 = (HtmlGenericControl)this.Page.Master.FindControl("livieword");
            HtmlGenericControl img3 = (HtmlGenericControl)this.Page.Master.FindControl("liviewitem");
            img1.Attributes.Add("style", "display:none");
            img2.Attributes.Add("style", "display:none");
            img3.Attributes.Add("style", "display:none");
            HtmlGenericControl img4 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton1");
            HtmlGenericControl img5 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton2");
            HtmlGenericControl img6 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton3");
            HtmlGenericControl img7 = (HtmlGenericControl)this.Page.Master.FindControl("lilistitem");
            img4.Attributes.Add("style", "display:none");
            img5.Attributes.Add("style", "display:none");
            img6.Attributes.Add("style", "display:none");
            img7.Attributes.Add("style", "display:none");
        }

        if (Session["view_order_entry_pages"] == null)
        {
            /*ImageButton img1 = (ImageButton)Master.FindControl("ImageButton1");
            ImageButton img2 = (ImageButton)Master.FindControl("ImageButton2");
            ImageButton img3 = (ImageButton)Master.FindControl("ImageButton3");

            ImageButton img11 = (ImageButton)Master.FindControl("listitem");
            img1.Visible = false;
            img2.Visible = false;
            img3.Visible = false;

            img11.Visible = false;*/
            HtmlGenericControl img1 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton1");
            HtmlGenericControl img2 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton2");
            HtmlGenericControl img3 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton3");
            HtmlGenericControl img11 = (HtmlGenericControl)this.Page.Master.FindControl("lilistitem");
            img1.Attributes.Add("style", "display:none");
            img2.Attributes.Add("style", "display:none");
            img3.Attributes.Add("style", "display:none");
            img11.Attributes.Add("style", "display:none");
        }

        if (Session["view_order_entry_pages"] != null)
        {
           /* ImageButton img1 = (ImageButton)Master.FindControl("brwsorder");
            ImageButton img2 = (ImageButton)Master.FindControl("vieworder");
            ImageButton img3 = (ImageButton)Master.FindControl("viewitem");

            img1.Visible = false;
            img2.Visible = false;
            img3.Visible = false;*/
            HtmlGenericControl img1 = (HtmlGenericControl)this.Page.Master.FindControl("librowseorder");
            HtmlGenericControl img2 = (HtmlGenericControl)this.Page.Master.FindControl("livieword");
            HtmlGenericControl img3 = (HtmlGenericControl)this.Page.Master.FindControl("liviewitem");
            img1.Attributes.Add("style", "display:none");
            img2.Attributes.Add("style", "display:none");
            img3.Attributes.Add("style", "display:none");


        }

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;


        if (Session["User"] != null)
        {


            string vUserId = UserLogin.UserName;
            string vPage = "releases.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);


            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }

            

                SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                try
                {
                    conn.Open();

                    string cmd = "select * from button_maintain where parent = 'order_estimate.aspx' ";
                    SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                    DataSet ds = new DataSet();
                    da.Fill(ds);

                    if (ds.Tables[0].Rows.Count == 0)
                    {
                        SqlCommand cmd_insert = new SqlCommand("insert into button_maintain (parent, name, btn1, btn2, btn3, btn4, btn5, btn6, btn7, btn8, btn9,btn10,chk1,chk2,chk3,chk4,chk5,chk6,chk7,chk8,chk9,chk10) values ('order_estimate.aspx','Order Entry & Order Status','View Order','List Item','View Item','Misc Chgs','Job Status','Releases','Order Total','Item Status','Invoices','Ship Notes','True','True','True','True','True','True','True','True','True','True')", conn);
                        cmd_insert.ExecuteNonQuery();
                    }

                    foreach (DataRow dr in ds.Tables[0].Rows)
                    {
                        if (Session["view_order_entry_pages_with_estimate"] == null)
                        {
                            //ImageButton img2 = (ImageButton)Master.FindControl("vieworder");
                            //ImageButton img3 = (ImageButton)Master.FindControl("viewitem");
                            HtmlGenericControl img2 = (HtmlGenericControl)this.Page.Master.FindControl("livieword");
                            HtmlGenericControl img3 = (HtmlGenericControl)this.Page.Master.FindControl("liviewitem");
                            string[] ss1 = dr["user1"].ToString().Split(',');
                            string[] ss3 = dr["user3"].ToString().Split(',');
                            if (ss1.Contains(UserLogin.UserName))
                            {
                                //img2.Visible = Convert.ToBoolean(dr["chk1"].ToString());
                                if (dr["chk1"].ToString() == "False")
                                    img2.Attributes.Add("style", "display:none");
                            }
                            if (ss3.Contains(UserLogin.UserName))
                            {
                                //img3.Visible = Convert.ToBoolean(dr["chk3"].ToString());
                                if (dr["chk3"].ToString() == "False")
                                    img3.Attributes.Add("style", "display:none");
                            }
                        }

                        if (Session["view_order_entry_pages_with_estimate"] != null)
                        {

                            //ImageButton img5 = (ImageButton)Master.FindControl("ImageButton5");
                            //ImageButton img6 = (ImageButton)Master.FindControl("ImageButton6");
                            //ImageButton img7 = (ImageButton)Master.FindControl("ImageButton7");
                            HtmlGenericControl img5 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton5");
                            HtmlGenericControl img6 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton6");
                            HtmlGenericControl img7 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton7");
                            string[] ss1 = dr["user1"].ToString().Split(',');
                            string[] ss2 = dr["user2"].ToString().Split(',');
                            string[] ss3 = dr["user3"].ToString().Split(',');
                            if (ss1.Contains(UserLogin.UserName))
                            {
                                //img5.Visible = Convert.ToBoolean(dr["chk1"].ToString());
                                if (dr["chk1"].ToString() == "False")
                                    img5.Attributes.Add("style", "display:none");
                            }
                            if (ss2.Contains(UserLogin.UserName))
                            {
                                //img6.Visible = Convert.ToBoolean(dr["chk2"].ToString());
                                if (dr["chk2"].ToString() == "False")
                                    img6.Attributes.Add("style", "display:none");
                            }
                            if (ss3.Contains(UserLogin.UserName))
                            {
                                //img7.Visible = Convert.ToBoolean(dr["chk3"].ToString());
                                if (dr["chk3"].ToString() == "False")
                                    img7.Attributes.Add("style", "display:none");
                            }
                        }

                    }
                    conn.Close();

                }
                catch { }
            

        }

        Label name = (Label)Master.FindControl("lbl_page");
        name.Text = "Releases";

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "";
        ObjectDataSource2.SelectParameters["prmAction"].DefaultValue = "";
        /*ImageButton releases = (ImageButton)Master.FindControl("releases");
        releases.ImageUrl = "~/img/releases1.jpg";*/
        FormView1.ChangeMode(FormViewMode.ReadOnly);


    }


    protected void Delete_release(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {


            string vUserId = UserLogin.UserName;
            string vPage = "releases.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;




            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
            //((Button)FormView1.FindControl("ReleaseButton")).Visible = false;
            //lblComp.Text = PrmComp;


            if (vCanDelete == false)
            {
                Response.Write("<script>alert('Sorry! You do not have permission Delete Record');</script>");
                Response.Write("<script>window.location.href = 'releases.aspx';</script>");

            }


        }

        if (Convert.ToString(Session["order_inquiry_status_check"]) == "P" || Convert.ToString(Session["order_entry_status_check"]) == "P")
        {

            Response.Write("<script>alert('Sorry! Cannot Modified , Allready Posted');</script>");
            Response.Write("<script>window.history.back()</script>");
        }

        if (Convert.ToString(Session["order_inquiry_status_check"]) == "Z" || Convert.ToString(Session["order_entry_status_check"]) == "Z")
        {
            Response.Write("<script>alert('Sorry! Cannot Modified ,Allready Invoiced');</script>");
            Response.Write("<script>window.history.back()</script>");
        }
        if (Convert.ToString(Session["order_inquiry_status_check"]) == "C" || Convert.ToString(Session["order_entry_status_check"]) == "C")
        {
            Response.Write("<script>alert('Sorry! Cannot Modified ,Allready closed');</script>");
            Response.Write("<script>window.history.back()</script>");
        }

        string swhere = Request.Form["Relradio"];
        if (swhere != "" && swhere != null)
        {
            string[] radioval = swhere.Split(new char[] { ',' });
            string status = radioval[1];

            if (status == "P")
            {

                Response.Write("<script>alert('Sorry! Cannot Delete, Allready Posted');</script>");
                Response.Write("<script>window.location.href = 'releases.aspx';</script>");
            }

            if (status == "Z")
            {
                Response.Write("<script>alert('Sorry! Cannot Delete, Allready Invoiced');</script>");
                Response.Write("<script>window.location.href = 'releases.aspx';</script>");
            }
            if (status == "C")
            {
                Response.Write("<script>alert('Sorry! Cannot Delete, Allready closed');</script>");
                Response.Write("<script>window.location.href = 'releases.aspx';</script>");
            }
            //ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "delete";
            //ObjectDataSource1.SelectParameters["vRowid"].DefaultValue = radioval[0];
            Response.Write(radioval[0]);

            Session["releases"] = Session["releases"];
            Session["line"] = Session["line"];
        }
        else
        {
            Response.Write("<script>alert('No Record to Delete')</script>");
        }
    }
    protected void Cancel_release(object sender, EventArgs e)
    {
        string swhere2 = Request.Form["ARowid"];

        if (swhere2 != "" && swhere2 != null)
        {
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "delete";
            ObjectDataSource1.SelectParameters["vRowid"].DefaultValue = swhere2;

            Session["releases"] = Session["releases"];
            Session["line"] = Session["line"];
        }

    }



    protected void Releaseitem(object sender, EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "releases.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;


            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
            //((Button)FormView1.FindControl("ReleaseButton")).Visible = false;
            //lblComp.Text = PrmComp;


            //if (vCanUpdate == false)
            //{
            //    Response.Write("<script>alert('Sorry! You do not have permission to Release Record');</script>");
            //    Response.Write("<script>window.location.href = 'releases.aspx';</script>");

            //}


        }
        if (Convert.ToString(Session["order_inquiry_status_check"])=="W" || Convert.ToString(Session["order_entry_status_check"])=="W")
        {
            Response.Write("<script>alert('Sorry! Web Orders cannot be Released Until Approved By  Vendo ');</script>");
            Response.Write("<script>window.history.back()</script>");
        }

        else if (Convert.ToString(Session["order_inquiry_status_check"])=="P" || Convert.ToString(Session["order_entry_status_check"])=="P")
        {
            Response.Write("<script>alert('Sorry! Cannot Released, Allready Posted');</script>");
            Response.Write("<script>window.history.back()</script>");
        }

        else if (Convert.ToString(Session["order_inquiry_status_check"]) == "Z" || Convert.ToString(Session["order_entry_status_check"]) == "Z")
        {
            Response.Write("<script>alert('Sorry! Cannot Released, Allready Invoiced');</script>");
            Response.Write("<script>window.history.back()</script>");
        }
        else if (Convert.ToString(Session["order_inquiry_status_check"]) == "C" || Convert.ToString(Session["order_entry_status_check"]) == "C")
        {
            Response.Write("<script>alert('Sorry! Cannot Released, Allready closed');</script>");
            Response.Write("<script>window.history.back()</script>");
        }
        
        else
        {
            string swhere = Request.Form["Relradio"];

            if (swhere != "" && swhere != null)
            {
                string[] radioval = swhere.Split(new char[] { ',' });
                string status = radioval[1];

                if (status == "P")
                {

                    Response.Write("<script>alert('Sorry! Cannot Released, Allready Posted');</script>");
                    Response.Write("<script>window.location.href = 'releases.aspx';</script>");
                }
                if (status == "A" )
                {

                    Response.Write("<script>alert('Actual Release entry can not be modified ');</script>");
                    Response.Write("<script>window.location.href = 'releases.aspx';</script>");
                }

                if (status == "Z")
                {
                    Response.Write("<script>alert('Sorry! Cannot Released, Allready Invoiced');</script>");
                    Response.Write("<script>window.location.href = 'releases.aspx';</script>");
                }
                if (status == "C")
                {
                    Response.Write("<script>alert('Sorry! Cannot Released, Allready closed');</script>");
                    Response.Write("<script>window.location.href = 'releases.aspx';</script>");
                }




                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Releaseitem";
                ObjectDataSource1.SelectParameters["vRowid"].DefaultValue = radioval[0];

                Session["releases"] = Session["releases"];
                Session["line"] = Session["line"];


                
            }
            else
            {
                Response.Write("<script>alert('No Record to Release')</script>");
            }



            //try
            //{
            //    MailMessage message = new MailMessage();
            //message.From = new MailAddress("sewa@networkindia.net");
            //message.To.Add(new MailAddress("sewa@networkindia.net"));


            //message.Subject = "This is my subject";

            //message.Body = "This is the content";



            //SmtpClient client = new SmtpClient();

            //client.Send(message);
            //}
            //catch (Exception ex) { } 
        }

    }
    //protected void Unpost_release(object sender, EventArgs e)
    //{
    //    string swhere = Request.Form["Relradio"];
    //    ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "unpostActual";
    //    ObjectDataSource1.SelectParameters["vRowid"].DefaultValue = swhere;

    //    Session["releases"] = Session["releases"];
    //    Session["line"] = Session["line"];


    //}

    protected void updateRelease(object sender, EventArgs e)
    {

        Label svalue = (Label)FormView1.FindControl("VstatusTextBox");
        string status = svalue.Text;

        //if (status == "A" || status == "B")
        //{
        //    //Response.Write("<script>window.showModalDialog('confirmdatechange.aspx','Date Change','dialogWidth=400px;dialogHeight=300px;centre=yes')</script>");
        //    TextBox TextBox2 = (TextBox)FormView1.FindControl("TextBox2");
        //    TextBox2.Visible = false;

        //}

        //if (status == "S" || status == "L" || status == "I")
        //{
        //    //Response.Write("<script>window.open('changecustomerpo.aspx')</script>");
        //    //Response.Write("<script>window.showModalDialog('changecustomerpo.aspx','customerpo','dialogWidth=500px;dialogHeight=200px;centre=yes');</script>");
        //    TextBox TextBox2 = (TextBox)FormView1.FindControl("TextBox2");
        //    TextBox2.Visible = true;
        //    TextBox2.Focus();
        //}

        // Response.Write(Session["Pochanged"]);
        //Response.Write(Session["Datechanged"]);
        //Response.Write(Session["AllDatechanged"]);






        string swhere1 = Request.Form["HiddenRowid"];

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "updateRelease";
        ObjectDataSource1.SelectParameters["vRowid"].DefaultValue = swhere1;

        //Response.Write(Session["releases"]);

        Session["releases"] = Session["releases"];
        Session["line"] = Session["line"];

        CheckBox checkheaderduedate = (CheckBox)FormView1.FindControl("CheckBox1");
        //CheckBox checkheaderlastshipdate = (CheckBox)FormView1.FindControl("CheckBox2");
        CheckBox checklineitemduedate = (CheckBox)FormView1.FindControl("CheckBox3");
        //CheckBox checklineitemlastshipdate = (CheckBox)FormView1.FindControl("CheckBox4");
        CheckBox checkreleasealldate = (CheckBox)FormView1.FindControl("CheckBox5");
        CheckBox checkallpo = (CheckBox)FormView1.FindControl("CheckBox6");
        CheckBox checkshipalldate = (CheckBox)FormView1.FindControl("CheckBox7");
        string headerduedate;
        //string headerlastshipdate;
        string lineitemduedate;
        //string lineitemlastshipdate;
        string releasealldate;
        string allpo;
        string shipalldate;
        if (checkheaderduedate.Checked)
        {
            headerduedate = "YES";
        }
        else
        {
            headerduedate = "NO";
        }
        //if (checkheaderlastshipdate.Checked)
        //{
        //    headerlastshipdate = "YES";
        //}
        //else
        //{
        //    headerlastshipdate = "NO";
        //}
        if (checklineitemduedate.Checked)
        {
            lineitemduedate = "YES";
        }
        else
        {
            lineitemduedate = "NO";
        }
        //if (checklineitemlastshipdate.Checked)
        //{
        //    lineitemlastshipdate = "YES";
        //}
        //else
        //{
        //    lineitemlastshipdate = "NO";
        //}
        if (checkreleasealldate.Checked)
        {
            releasealldate = "YES";
        }
        else
        {
            releasealldate = "NO";
        }
        if (checkallpo.Checked)
        {
            allpo = "YES";
        }
        else
        {
            allpo = "NO";
        }
        if (checkshipalldate.Checked)
        {
            shipalldate = "YES";
        }
        else
        {
            shipalldate = "NO";
        }

        //Response.Write(headerduedate);
        //Response.Write(headerlastshipdate);
        //Response.Write(lineitemduedate);
        //Response.Write(lineitemlastshipdate);
        //Response.Write(releasealldate);
        //Response.Write(allpo);
        //Response.Write(shipalldate);

        DropDownList Asi = (DropDownList)FormView1.FindControl("SITextBox");
        TextBox AShipTo = (TextBox)FormView1.FindControl("ShipToTextBox");
        TextBox AVia = (TextBox)FormView1.FindControl("ViaTextBox");
        TextBox ASqty = (TextBox)FormView1.FindControl("SQtyTextBox");
        TextBox ApoNo = (TextBox)FormView1.FindControl("po_noTextBox");
        TextBox AlotNo = (TextBox)FormView1.FindControl("lot_noTextBox");
        TextBox ADate = (TextBox)FormView1.FindControl("MyDate");
        TextBox sellPrice = (TextBox)FormView1.FindControl("sellPrice");
        TextBox Afrtpay = (TextBox)FormView1.FindControl("frtpayLabel");
        TextBox Afob = (TextBox)FormView1.FindControl("fobLabel");

        Session["datechange"] = ADate.Text;
        //Response.Write(Session["datechange"]);

        ObjectDataSource1.SelectParameters["Asi"].DefaultValue = Asi.SelectedValue.Trim();
        ObjectDataSource1.SelectParameters["AShipTo"].DefaultValue = AShipTo.Text.Trim();
        ObjectDataSource1.SelectParameters["AVia"].DefaultValue = AVia.Text.Trim();
        ObjectDataSource1.SelectParameters["ASqty"].DefaultValue = ASqty.Text.Trim();
        ObjectDataSource1.SelectParameters["ApoNo"].DefaultValue = ApoNo.Text.Trim();
        ObjectDataSource1.SelectParameters["AlotNo"].DefaultValue = AlotNo.Text.Trim();
        ObjectDataSource1.SelectParameters["ADate"].DefaultValue = ADate.Text.Trim();
        //ObjectDataSource1.SelectParameters["sellPrice"].DefaultValue = sellPrice.Text.Trim();
        ObjectDataSource1.SelectParameters["HeaderDueDate"].DefaultValue = headerduedate;
        //ObjectDataSource1.SelectParameters["headerlastshipdate"].DefaultValue = headerlastshipdate;
        ObjectDataSource1.SelectParameters["lineitemduedate"].DefaultValue = lineitemduedate;
        //ObjectDataSource1.SelectParameters["lineitemlastshipdate"].DefaultValue = lineitemlastshipdate;
        ObjectDataSource1.SelectParameters["RelAllDt"].DefaultValue = releasealldate;
        ObjectDataSource1.SelectParameters["AllPo"].DefaultValue = allpo;
        ObjectDataSource1.SelectParameters["ShipAllDate"].DefaultValue = shipalldate;
        ObjectDataSource1.SelectParameters["Afrtpay"].DefaultValue = Afrtpay.Text.Trim();
        ObjectDataSource1.SelectParameters["Afob"].DefaultValue = Afob.Text.Trim();


        //Session["headerduedate"] = null;
        //Session["headerlastshipdate"] = null;
        //Session["lineitemduedate"] = null;
        //Session["lineitemlastshipdate"] = null;

        //Session["Pochanged"] = null;
        //Session["Datechanged"] = null;
        //Session["AllDatechanged"] = null;

    }
    protected void updaterel(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {


            string vUserId = UserLogin.UserName;
            string vPage = "releases.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;




            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
            //((Button)FormView1.FindControl("ReleaseButton")).Visible = false;
            //lblComp.Text = PrmComp;


            if (vCanUpdate == false)
            {
                Response.Write("<script>alert('Sorry! You do not have permission to Update Record');</script>");
                Response.Write("<script>window.location.href = 'releases.aspx';</script>");

            }


        }

        if (Convert.ToString(Session["order_inquiry_status_check"]) == "A" || Convert.ToString(Session["order_entry_status_check"]) == "A")
        {

            Response.Write("<script>alert('Sorry! Cannot Modified ');</script>");
            Response.Write("<script>window.history.back()</script>");
        }

        if (Convert.ToString(Session["order_inquiry_status_check"]) == "B" || Convert.ToString(Session["order_entry_status_check"]) == "B")
        {
            Response.Write("<script>alert('Sorry! Cannot Modified ');</script>");
            Response.Write("<script>window.history.back()</script>");
        }
        if (Convert.ToString(Session["order_inquiry_status_check"]) == "P" || Convert.ToString(Session["order_entry_status_check"]) == "P")
        {

            Response.Write("<script>alert('Sorry! Cannot Modified , Allready Posted');</script>");
            Response.Write("<script>window.history.back()</script>");
        }

        if (Convert.ToString(Session["order_inquiry_status_check"]) == "Z" || Convert.ToString(Session["order_entry_status_check"]) == "Z")
        {
            Response.Write("<script>alert('Sorry! Cannot Modified ,Allready Invoiced');</script>");
            Response.Write("<script>window.history.back()</script>");
        }
        if (Convert.ToString(Session["order_inquiry_status_check"]) == "C" || Convert.ToString(Session["order_entry_status_check"]) == "C")
        {
            Response.Write("<script>alert('Sorry! Cannot Modified ,Allready closed');</script>");
            Response.Write("<script>window.history.back()</script>");
        }



        string swhere = Request.Form["Relradio"];
        if (swhere != "" && swhere != null)
        {

            string[] radioval = swhere.Split(new char[] { ',' });

            string status = radioval[1];
            if (status == "A" || status == "W")
            {

                Response.Write("<script>alert('Sorry! Cannot Modified ');</script>");
                Response.Write("<script>window.location.href = 'releases.aspx';</script>");
            }

            if (status == "B")
            {
                Response.Write("<script>alert('Sorry! Cannot Modified ');</script>");
                Response.Write("<script>window.location.href = 'releases.aspx';</script>");
            }
            if (status == "P")
            {

                Response.Write("<script>alert('Sorry! Cannot Modified , Allready Posted');</script>");
                Response.Write("<script>window.location.href = 'releases.aspx';</script>");
            }

            if (status == "Z")
            {
                Response.Write("<script>alert('Sorry! Cannot Modified ,Allready Invoiced');</script>");
                Response.Write("<script>window.location.href = 'releases.aspx';</script>");
            }
            if (status == "C")
            {
                Response.Write("<script>alert('Sorry! Cannot Modified ,Allready closed');</script>");
                Response.Write("<script>window.location.href = 'releases.aspx';</script>");
            }

            ObjectDataSource2.SelectParameters["prmAction"].DefaultValue = "SelectRow";
            ObjectDataSource2.SelectParameters["vRowid"].DefaultValue = radioval[0];
            //Response.Write(Session["aRowid"]);
            Session["releases"] = Session["releases"];
            Session["line"] = Session["line"];
        }
        else
        {
            Response.Write("<script>alert('No Record to Update')</script>");

            Response.Write("<script>window.location.href = 'releases.aspx';</script>");

        }



    }
    protected void AddRelease(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {


            string vUserId = UserLogin.UserName;
            string vPage = "releases.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;




            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
            //((Button)FormView1.FindControl("ReleaseButton")).Visible = false;
            //lblComp.Text = PrmComp;


            if (vCanCreate == false)
            {
                Response.Write("<script>alert('Sorry! You do not have permission to Add new Record');</script>");
                Response.Write("<script>window.location.href = 'releases.aspx';</script>");

            }


        }

        //string swhere = Request.Form["Relradio"];
        //string[] radioval = swhere.Split(new char[] { ',' });


        ObjectDataSource2.SelectParameters["prmAction"].DefaultValue = "AddRelease1";
        //ObjectDataSource2.SelectParameters["vRowid"].DefaultValue = radioval[0];
        Session["releases"] = Session["releases"];
        Session["line"] = Session["line"];
        //Response.Write(Session["releases"]);
        //Response.Write(swhere);

    }
    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        try
        {
            if (FormView1.CurrentMode == FormViewMode.Edit)
            {
                TextBox ship = (TextBox)FormView1.FindControl("ShipToTextBox");

                if (ship.Text.Trim() != "")
                {
                    Session["rel_carrier_lookup_val"] = ship.Text;
                }
                else
                {
                    Session["rel_carrier_lookup_val"] = null;
                }
            }
        }

        catch { return; }
    }
    protected void BolInv_Click(object sender, EventArgs e)
    {
        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        string swhere = Request.Form["Relradio"];
        if (swhere != "" && swhere != null)
        {
            string[] radioval = swhere.Split(new char[] { ',' });
            
            Order ord = new Order();

            bool check = ord.ValidateRelease(UserLogin.UserName, Convert.ToString(Session["releases"]), Convert.ToString(Session["line"]), "ValidateBolinv", Convert.ToInt64(radioval[0]), "", "", "", "", "", "", Convert.ToDateTime("12/12/2011"), "", "", "", "", "", "", "", "", "", "");

            string value = Convert.ToString(check);
            if (value == "True")
            {                
                ObjectDataSource2.SelectParameters["prmAction"].DefaultValue = "Bolinvcreate";
                ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource2.SelectParameters["vRowid"].DefaultValue = radioval[0];
                FormView1.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href = 'releases.aspx';</script>");

                

            }
        }
        
               
        
    }


}
