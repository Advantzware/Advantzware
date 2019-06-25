using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Data.SqlClient;
using System.Linq;

/// <summary>
/// Summary description for vieworder
/// </summary>
public partial class vieworder : System.Web.UI.Page
{
    public vieworder()
    {
        //
        // TODO: Add constructor logic here
        //
    }
    protected void Page_Load(object sender, EventArgs e)
    {
        if (Session["view_order_entry_pages_with_estimate"] == null)
        {
            HtmlGenericControl img4 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton4");
            HtmlGenericControl img5 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton5");
            HtmlGenericControl img6 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton6");
            HtmlGenericControl img7 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton7");
            img4.Attributes.Add("style", "display:none");
            img5.Attributes.Add("style", "display:none");
            img6.Attributes.Add("style", "display:none");
            img7.Attributes.Add("style", "display:none");
            //ImageButton img4 = (ImageButton)Master.FindControl("ImageButton4");
            //ImageButton img5 = (ImageButton)Master.FindControl("ImageButton5");
            //ImageButton img6 = (ImageButton)Master.FindControl("ImageButton6");
            //ImageButton img7 = (ImageButton)Master.FindControl("ImageButton7");
            //img4.Visible = false;
            //img5.Visible = false;
            //img6.Visible = false;
            //img7.Visible = false;

            Image ack = (Image)Master.FindControl("Image2");
            ImageButton add = (ImageButton)Master.FindControl("img_btn_add");
            //ack.Visible = false;
            add.Visible = false;
        }

        if (Session["view_order_entry_pages_with_estimate"] != null)
        {
            HtmlGenericControl img1 = (HtmlGenericControl)this.Page.Master.FindControl("librowseorder");
            HtmlGenericControl img2 = (HtmlGenericControl)this.Page.Master.FindControl("livieword");
            HtmlGenericControl img3 = (HtmlGenericControl)this.Page.Master.FindControl("liviewitem");
            img1.Attributes.Add("style", "display:none");
            img2.Attributes.Add("style", "display:none");
            img3.Attributes.Add("style", "display:none");
            /*ImageButton img1 = (ImageButton)Master.FindControl("brwsorder");
            ImageButton img2 = (ImageButton)Master.FindControl("vieworder");
            ImageButton img3 = (ImageButton)Master.FindControl("viewitem");

            img1.Visible = false;
            img2.Visible = false;
            img3.Visible = false;*/

            /*ImageButton img4 = (ImageButton)Master.FindControl("ImageButton1");
            ImageButton img5 = (ImageButton)Master.FindControl("ImageButton2");
            ImageButton img6 = (ImageButton)Master.FindControl("ImageButton3");

            ImageButton img11 = (ImageButton)Master.FindControl("listitem");
            img4.Visible = false;
            img5.Visible = false;
            img6.Visible = false;

            img11.Visible = false;*/

            HtmlGenericControl img4 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton1");
            HtmlGenericControl img5 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton2");
            HtmlGenericControl img6 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton3");
            HtmlGenericControl img11 = (HtmlGenericControl)this.Page.Master.FindControl("lilistitem");
            img4.Attributes.Add("style", "display:none");
            img5.Attributes.Add("style", "display:none");
            img6.Attributes.Add("style", "display:none");
            img11.Attributes.Add("style", "display:none");
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

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;

        if (Session["User"] != null)
        {


            string vUserId = UserLogin.UserName;
            string vPage = "vieworder.aspx";
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

           
                //price_button.Visible = false;
                
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
                            ImageButton img2 = (ImageButton)Master.FindControl("vieworder");
                            ImageButton img3 = (ImageButton)Master.FindControl("viewitem");
                                                       
                            string[] ss1 = dr["user1"].ToString().Split(',');
                            string[] ss3 = dr["user3"].ToString().Split(',');
                            if (ss1.Contains(UserLogin.UserName))
                            {
                                img2.Visible = Convert.ToBoolean(dr["chk1"].ToString());
                            }
                            if (ss3.Contains(UserLogin.UserName))
                            {
                                img3.Visible = Convert.ToBoolean(dr["chk3"].ToString());
                            }
                        }

                        if (Session["view_order_entry_pages_with_estimate"] != null)
                        {

                            ImageButton img5 = (ImageButton)Master.FindControl("ImageButton5");
                            ImageButton img6 = (ImageButton)Master.FindControl("ImageButton6");
                            ImageButton img7 = (ImageButton)Master.FindControl("ImageButton7");
                            string[] ss1 = dr["user1"].ToString().Split(',');
                            string[] ss2 = dr["user2"].ToString().Split(',');
                            string[] ss3 = dr["user3"].ToString().Split(',');
                            if (ss1.Contains(UserLogin.UserName))
                            {
                                img5.Visible = Convert.ToBoolean(dr["chk1"].ToString());
                            }
                            if (ss2.Contains(UserLogin.UserName))
                            {
                                img6.Visible = Convert.ToBoolean(dr["chk2"].ToString());
                            }
                            if (ss3.Contains(UserLogin.UserName))
                            {
                                img7.Visible = Convert.ToBoolean(dr["chk3"].ToString());
                            }
                        }

                    }
                    conn.Close();


                }
                catch { }
            


        }
        Label name = (Label)Master.FindControl("lbl_page");
        name.Text = "View Order";
        /*ImageButton vieworder = (ImageButton)Master.FindControl("vieworder");
        vieworder.ImageUrl = "~/img/vieworder1.jpg";*/
    }

    protected string GenerateBillCityLine(object dataItem)
    {
        return (string)DataBinder.Eval(dataItem, "city")
            + ", " + (string)DataBinder.Eval(dataItem, "state")
            + " " + (string)DataBinder.Eval(dataItem, "zip");
    }

    protected string GenerateSoldCityLine(object dataItem)
    {
        return (string)DataBinder.Eval(dataItem, "[sold-city]")
            + ", " + (string)DataBinder.Eval(dataItem, "[sold-state]")
            + " " + (string)DataBinder.Eval(dataItem, "[sold-zip]");
    }
}
