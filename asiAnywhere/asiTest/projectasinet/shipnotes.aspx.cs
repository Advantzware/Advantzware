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
using System.Data.SqlClient;
using System.Linq;

public partial class shipnotesaspx : System.Web.UI.Page
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
            /*ImageButton img1 = (ImageButton)Master.FindControl("brwsorder");
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

        if (Session["User"] != null)
        {
            UserClass UserLogin = (UserClass)Session["User"];
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;

            UserClass.CheckLogin(Page);
            string vUserId = UserLogin.UserName;
            string vPage = "shipnotes.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
            //lblComp.Text = PrmComp;
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
        /*ImageButton shipnotes = (ImageButton)Master.FindControl("shipnotes");
        shipnotes.ImageUrl = "~/img/shipnotes1.jpg";*/

    }
    protected void FormView2_ItemUpdated(object sender, FormViewUpdatedEventArgs e)
    {


        this.FormView2.DataBind();
    }

    protected void UpdateBotton_Click(object sender, EventArgs e)
    {

        TextBox note1 = (TextBox)FormView2.FindControl("SNote1TextBox");
        TextBox note2 = (TextBox)FormView2.FindControl("SNote2TextBox");
        TextBox note3 = (TextBox)FormView2.FindControl("SNote3TextBox");
        TextBox note4 = (TextBox)FormView2.FindControl("SNote4TextBox");
        
        UserClass UserLogin = (UserClass)Session["User"];

            ObjectDataSource2.SelectParameters["prmAction"].DefaultValue = "Update";
            //ObjectDataSource3.SelectParameters["prmComp"].DefaultValue =  ; 
            ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource2.SelectParameters["prmNote1"].DefaultValue = note1.Text.Trim(); 
            ObjectDataSource2.SelectParameters["prmNote2"].DefaultValue = note2.Text.Trim();
            ObjectDataSource2.SelectParameters["prmNote3"].DefaultValue = note3.Text.Trim();
            ObjectDataSource2.SelectParameters["prmNote4"].DefaultValue = note4.Text.Trim();       
            

            FormView2.ChangeMode(FormViewMode.ReadOnly);
            /*Response.Write("<script>window.location.href='view_cash_rcpt.aspx'</script>");*/
        
    }

    protected void OnDataBound_FormView2(object sender, EventArgs e)
    {
        if (FormView2.CurrentMode == FormViewMode.ReadOnly)
        {
            Button updatebutton = (Button)FormView2.FindControl("UpdateItemButton");
            if (Session["view_order_entry_pages_with_estimate"] == null)
            {
                updatebutton.Visible = false;
            }
            else
                updatebutton.Visible = true;


        }
    }
}
